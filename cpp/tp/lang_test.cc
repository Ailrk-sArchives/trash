#include "lang.h"

template <typename N>
struct fact //
    : if_<typename ord<int_tag>::less::apply<N, int_<1>>::type,
          int_<1>,                                    //
          num<int_tag>::times::apply<                 //
              N,                                      //
              num<int_tag>::minus::apply<N, int_<1>>> //
          >::type {};

int main(void) { return 0; }
