#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim void caml_hello(value name)
{
    CAMLparam1(name);
   
    printf("Hello %s\n", String_val(name));

    CAMLreturn0;
}
