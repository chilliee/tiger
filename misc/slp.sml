signature STRAIGHTLINEPROG = sig

  datatype binop = Plus | Minus | Times | Div

  type id = string

  datatype stm = CompoundStm of stm * stm
               | AssignStm of id * exp
               | PrintStm of exp list

       and exp = IdExp of id
               | NumExp of int
               | OpExp of exp * binop * exp
               | EseqExp of stm * exp
  
  val maxargs : stm -> int

  val interp : stm -> unit

end

structure StraightLineProg :> STRAIGHTLINEPROG = struct

  datatype binop = Plus | Minus | Times | Div

  type id = string

  datatype stm = CompoundStm of stm * stm
               | AssignStm of id * exp
               | PrintStm of exp list

       and exp = IdExp of id
               | NumExp of int
               | OpExp of exp * binop * exp
               | EseqExp of stm * exp
  
  (* maxargs *)
  fun max (x:int, y:int) : int = if x < y then y else x

  fun length nil = 0
    | length (_::L) = 1 + length L
  
  fun maxargs (s:stm) : int =
    case s
      of CompoundStm (s1, s2) => max (maxargs s1, maxargs s2)
       | AssignStm (_, e) => maxargs_exp e
       | PrintStm el => max (length el, maxargs_explist (el, 0))
  
  and maxargs_exp (e:exp) : int =
    case e
      of IdExp _ => 0
       | NumExp _ => 0
       | OpExp (e1, _, e2) => max (maxargs_exp e1, maxargs_exp e2)
       | EseqExp (s, e) => max (maxargs s, maxargs_exp e)
  
  and maxargs_explist (el:exp list, r:int) : int =
    case el
      of nil => r
       | e::el' => maxargs_explist(el', max (r, maxargs_exp e))
  
  (* interp *)
  type table = (id * int) list

  fun update (t:table, i:id, v:int) : table = (i, v) :: t

  fun lookup (t:table, i:id) : int =
    case t
      of nil => 0
       | (i', v)::t' => if i' = i then v else lookup (t', i)
  
  fun interpStm (s:stm, t:table) : table =
    case s
      of CompoundStm (s1, s2) => interpStm (s2, interpStm (s1, t))
       | AssignStm (i, e) =>
          let
              val (v, t') = interpExp (e, t)
          in
              update (t', i, v)
          end
       | PrintStm nil  => t
       | PrintStm (e::nil) =>
          let
              val (v, t') = interpExp (e, t)
          in
              (print (Int.toString v ^ "\n"); t')
          end
       | PrintStm (e::el) =>
          let
              val (v, t') = interpExp (e, t)
          in
              (print (Int.toString v ^ " "); interpStm (PrintStm el, t'))
          end
  
  and interpExp (e:exp, t:table) : int * table =
    case e
      of IdExp i => (lookup(t, i), t)
       | NumExp v => (v, t)
       | OpExp (e1, b, e2) =>
          let
              val (v1, t') = interpExp (e1, t)
              val (v2, t'') = interpExp (e2, t')
          in
              case b
                of Plus => ((v1 + v2), t'')
                 | Minus => ((v1 - v2), t'')
                 | Times => ((v1 * v2), t'')
                 | Div => ((v1 div v2), t'')
          end
       | EseqExp (s', e') => interpExp (e', interpStm(s', t))
  
  fun interp (s:stm) : unit = (interpStm (s, nil); ())
end

val prog = 
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
  PrintStm[IdExp "b"]))