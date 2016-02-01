


def prod(f: Int => Int) (a: Int, b: Int) : Int = {
    apply((x,y)=>x*y)(f)(a,b)
}

def fact(n: Int) = prod(x=>x)(1,n)

prod(x => x)(1, 3)

fact(5)


def apply(g: (Int, Int) => Int)(f: Int => Int) (a: Int, b: Int) : Int = {
  if (a>b) 1 else
    g(f(a), apply(g)(f)(a+1, b))
}