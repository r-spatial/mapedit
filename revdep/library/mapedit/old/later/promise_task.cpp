#include <Rcpp.h>
#include <later_api.h>

class PromiseTask : public later::BackgroundTask {
public:
  PromiseTask(Rcpp::Function resolve, Rcpp::Function reject)
      : resolve(resolve), reject(reject) {
  }

protected:
  virtual void execute() = 0;
  virtual Rcpp::RObject get_result() = 0;

  void complete() {
    Rcpp::RObject result = get_result();
    resolve(result);
  }

private:
  Rcpp::Function resolve;
  Rcpp::Function reject;
};

long fib(long x) {
  if (x <= 2) {
    return 1;
  }
  return fib(x - 1) + fib(x - 2);
}

class FibonacciTask : public PromiseTask {
public:
  FibonacciTask(Rcpp::Function resolve, Rcpp::Function reject, double x)
      : PromiseTask(resolve, reject), x(x) {
  }

  void execute() {
    result = fib((long)x);
  }

  Rcpp::RObject get_result() {
    Rcpp::NumericVector res(1);
    res[0] = (double)result;
    return res;
  }

private:
  double x;
  long result;
};

// [[Rcpp::depends(later)]]
// [[Rcpp::export]]
void asyncFib(Rcpp::Function resolve, Rcpp::Function reject, double x) {
  FibonacciTask* fib = new FibonacciTask(resolve, reject, x);
  fib->begin();
}

/* R
library(promises)
library(later)

Rcpp::sourceCpp(system.file("promise_task.cpp", package = "later"))

promise(function(resolve, reject) {
  asyncFib(resolve, reject, 45)
}) |>
  then(print)

 */
