#ifndef _later_later_api_h
#define _later_later_api_h

// ---- Platform prerequisites ------------------------------------------------
// System and R headers required by the later API.

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#ifndef STRICT_R_HEADERS
#define STRICT_R_HEADERS
#endif

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600 // so R <= 4.1 can find WSAPoll() on Windows
#endif
#include <winsock2.h>
#define WIN32_LEAN_AND_MEAN
// Taken from http://tolstoy.newcastle.edu.au/R/e2/devel/06/11/1242.html
// Undefine the Realloc macro, which is defined by both R and by Windows stuff
#undef Realloc
// Also need to undefine the Free macro
#undef Free
#include <windows.h>
#else // _WIN32
#include <poll.h>
#include <pthread.h>
#endif // _WIN32

#include <Rinternals.h>

// ---- Public API ------------------------------------------------------------
// The later C++ interface. See the "Using later from C++" vignette for usage.

namespace later {

// This is the version of the later API provided by this file. Ideally, this
// should match the version of the API provided by the later DLL that is
// installed on the user's system. However, since this file is compiled into
// other packages (like httpuv and promises), it is possible that there will
// be a mismatch. In the future we will be able to compare at runtime it to
// the result from apiVersion(), with:
//
// int (*dll_api_version)() = (int (*)()) R_GetCCallable("later", "apiVersion");
// if (LATER_H_API_VERSION != (*dll_api_version)()) { ... }
#define LATER_H_API_VERSION 3
#define GLOBAL_LOOP 0


// Gets the version of the later API that's provided by the _actually installed_
// version of later.
// # nocov start
// tested by cpp-version-mismatch job on CI
static int apiVersionRuntime() {
  int (*dll_api_version)(void) = (int (*)(void)) R_GetCCallable("later", "apiVersion");
  return (*dll_api_version)();
}
// # nocov end

// ---- later() ---------------------------------------------------------------
// Schedule a C function to execute on the main R thread after a delay.
// Safe to call from any thread.

inline void later(void (*func)(void*), void* data, double secs, int loop_id) {
  // This function works by retrieving the later::execLaterNative2 function
  // pointer using R_GetCCallable the first time it's called (per compilation
  // unit, since it's inline). execLaterNative2 is designed to be safe to call
  // from any thread, but R_GetCCallable is only safe to call from R's main
  // thread (otherwise you get stack imbalance warnings or worse). Therefore,
  // we have to ensure that the first call to execLaterNative2 happens on the
  // main thread. We accomplish this using the statically initialized
  // LaterInitializer object at the end of this file.
  //
  // This file is named later_api.h instead of later.h because Rcpp treats
  // $PACKAGE.h files specially by including them in RcppExports.cpp, and we
  // definitely do not want the static initialization to happen there.

  // The function type for the real execLaterNative2
  typedef void (*elnfun)(void (*func)(void*), void*, double, int);
  static elnfun eln = NULL;
  if (!eln) {
    // Initialize if necessary
    if (func) {
      // We're not initialized but someone's trying to actually schedule
      // some code to be executed!
      REprintf(
        "Warning: later::execLaterNative2 called in uninitialized state.\n"
      );
    }
    eln = (elnfun)R_GetCCallable("later", "execLaterNative2");
  }

  // We didn't want to execute anything, just initialize
  if (!func) {
    return;
  }

  eln(func, data, secs, loop_id);
}

inline void later(void (*func)(void*), void* data, double secs) {
  later(func, data, secs, GLOBAL_LOOP);
}

// ---- later_fd() ------------------------------------------------------------
// Schedule a C function with file descriptor polling. Safe to call from any
// thread. Requires later >= 1.4.1 (API version 3).

// # nocov start
// tested by cpp-version-mismatch job on CI
static void later_fd_version_error(void (*func)(int *, void *), void *data, int num_fds, struct pollfd *fds, double secs, int loop_id) {
  (void) func; (void) data; (void) num_fds; (void) fds; (void) secs; (void) loop_id;
  Rf_error("later_fd called, but installed version of the 'later' package is too old; please upgrade 'later' to 1.4.1 or above");
}
// # nocov end

inline void later_fd(void (*func)(int *, void *), void *data, int num_fds, struct pollfd *fds, double secs, int loop_id) {
  // See above note for later()

  // The function type for the real execLaterFdNative
  typedef void (*elfdnfun)(void (*)(int *, void *), void *, int, struct pollfd *, double, int);
  static elfdnfun elfdn = NULL;
  if (!elfdn) {
    // Initialize if necessary
    if (func) {
      // We're not initialized but someone's trying to actually schedule
      // some code to be executed!
      REprintf(
        "Warning: later::execLaterFdNative called in uninitialized state.\n"
      );
    }
    if (apiVersionRuntime() >= 3) {
      // Only later API version 3 supports execLaterFdNative
      elfdn = (elfdnfun) R_GetCCallable("later", "execLaterFdNative");
    } else {
      // The installed version is too old and doesn't offer execLaterFdNative.
      elfdn = later_fd_version_error;
    }
  }

  // We didn't want to execute anything, just initialize
  if (!func) {
    return;
  }

  elfdn(func, data, num_fds, fds, secs, loop_id);
}

inline void later_fd(void (*func)(int *, void *), void *data, int num_fds, struct pollfd *fds, double secs) {
  later_fd(func, data, num_fds, fds, secs, GLOBAL_LOOP);
}


// ---- BackgroundTask --------------------------------------------------------
// Helper class for running work on a background thread and returning results
// on the main R thread. Subclass and implement execute() and complete().

class BackgroundTask {

public:
  BackgroundTask() {}
  virtual ~BackgroundTask() {}

  // Start executing the task
  void begin() {
#ifndef _WIN32
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_t t;
    pthread_create(&t, &attr, BackgroundTask::task_main, this);
    pthread_attr_destroy(&attr);
#else
    HANDLE hThread = ::CreateThread(
      NULL, 0,
      BackgroundTask::task_main_win,
      this,
      0,
      NULL
    );
    ::CloseHandle(hThread);
#endif
  }

protected:
  // The task to be executed on the background thread.
  // Neither the R runtime nor any R data structures may be
  // touched from the background thread; any values that need
  // to be passed into or out of the Execute method must be
  // included as fields on the Task subclass object.
  virtual void execute() = 0;

  // A short task that runs on the main R thread after the
  // background task has completed. It's safe to access the
  // R runtime and R data structures from here.
  virtual void complete() = 0;

private:
  static void* task_main(void* data) {
    BackgroundTask* task = reinterpret_cast<BackgroundTask*>(data);
    // TODO: Error handling
    task->execute();
    later(&BackgroundTask::result_callback, task, 0);
    return NULL;
  }

#ifdef _WIN32
  static DWORD WINAPI task_main_win(LPVOID lpParameter) {
    task_main(lpParameter);
    return 1;
  }
#endif

  static void result_callback(void* data) {
    BackgroundTask* task = reinterpret_cast<BackgroundTask*>(data);
    // TODO: Error handling
    task->complete();
    delete task;
  }
};

} // namespace later

// ---- Static initialization -------------------------------------------------
// Ensures later() and later_fd() are initialized on the main R thread before
// any user code can call them from a background thread.

namespace {

class LaterInitializer {
public:
  LaterInitializer() {
    // See comment in later() to learn why we need to do this
    // in a statically initialized object
    later::later(NULL, NULL, 0);
    later::later_fd(NULL, NULL, 0, NULL, 0);
  }
};

static LaterInitializer init;

} // namespace

#endif
