### Notes
The implementation is part of the exercises from 
[Types and Programming Languages by Benjamin Pierce](http://google.com/search?)

#### Ocaml/Scala translation notes.

##### Variable names
Ocaml allows variables to be named "'", we have chosen to convert such names to "_".

##### Function names (use camel case where possible)
The implementation has function names such as "isnumericval" which is probably better named as isNumericVal. 

##### Case statements
Case statements in ocaml have 
```

let rec isval ctx t = match t with
  | t when isnumericval ctx t  -> true
  | _ -> false
```

is converted in Scala as follows
```
  def isVal (ctx : Context) (t : Term) : Boolean = 
    t match {
      case TmFloat (_, _) if (isNumericVal(ctx)(t1)) => true
      case _ => false
    }
```

