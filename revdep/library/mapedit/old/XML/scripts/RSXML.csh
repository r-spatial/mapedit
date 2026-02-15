if(`test -n "-lxml2"`) then

if(${?LD_LIBRARY_PATH}) then
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:-lxml2
else
   setenv LD_LIBRARY_PATH -lxml2
endif

endif
