﻿namespace Monad 
module Monad = 
    #nowarn "1189"
   
    /// Given a readonly state, produces a value
    type Reader<'TState, 'T> = 'TState -> 'T
    /// Produces a value together with additional state
    type Writer<'TState, 'T> = 'TState * 'T
    /// Given state, produces new state & a value
    type State<'TState, 'T>  = 'TState -> 'TState * 'T

    type ReaderUpdate = 
        | NoUpdate
        static member Unit = NoUpdate
        static member Combine(NoUpdate, NoUpdate) = NoUpdate
        static member Apply(s, NoUpdate) = s
    /// Represents an update monad - given a state, produce 
    /// value and an update that can be applied to the state
    type UpdateMonad<'TState, 'TUpdate, 'T> = 
        UM of ('TState -> 'TUpdate * 'T)

    /// Returns the value of 'Unit' property on the ^S type
    let inline unit< ^S when ^S : (static member Unit : ^S)> () : ^S = (^S : (static member Unit : ^S) ()) 

    /// Invokes Combine operation on a pair of ^S values
    let inline (++)< ^S when ^S : (static member Combine : ^S * ^S -> ^S )> a b : ^S = (^S : (static member Combine : ^S * ^S -> ^S) (a, b)) 

    /// Invokes Apply operation on state and update ^S * ^U
    let inline apply< ^S, ^U when ^U : (static member Apply : ^S * ^U -> ^S )> s a : ^S = (^U : (static member Apply : ^S * ^U -> ^S) (s, a)) 

    type UpdateBuilder() = 
        /// Returns the specified value, together
        /// with empty update obtained using 'unit'
        member inline x.Return(v) : UpdateMonad<'S, 'U, 'T> = 
            UM (fun s -> (unit(),v))

        /// Compose two update monad computations
        member inline x.Bind(UM u1, f:'T -> UpdateMonad<'S, 'U, 'R>) =  
            UM (fun s -> 
                // Run the first computation to get first update
                // 'u1', then run 'f' to get second computation
                let (u1, x) = u1 s
                let (UM u2) = f x
                // Apply 'u1' to original state & run second computation
                // then return result with combined state updates
                let (u2, y) = u2 (apply s u1)
                (u1 ++ u2, y))
        member x.Map(f:'S->'R,a:UpdateMonad<'S, 'U, 'R>) =
            //let fR = f >> x.Return
            x.Bind (a, f >> x.Return)
    /// Instance of the computation builder
    /// that defines the update { .. } block
    let update = UpdateBuilder()
   
    /// Trivial monoid of updates 

    /// Read the current state (int) and return it as 'int'
    let read = UM (fun (s) -> (NoUpdate, s))
    /// Run computation and return the result 
    let readRun (s) (UM f) = f s |> snd
   
    /// Writer monad has no readable state
    type WriterState = NoState

    /// Updates of writer monad form a list
    type WriterUpdate<'TLog> = 
        | Log of list<'TLog>
        /// Returns the empty log (monoid unit)
        static member Unit = Log []
        /// Combines two logs (operation of the monoid)
        static member Combine(Log a, Log b) = Log(List.append a b)
        /// Applying updates to state does not affect the state
        static member Apply(NoState, _) = NoState
   
    /// Writes the specified value to the log 
    let write v = UM (fun s -> (Log [v], ()))
    /// Runs a "writer monad computation" and returns 
    /// the log, together with the final result
    let writeRun (UM f) = let (Log l, v) = f NoState in l, v

    /// Extends UpdateBuilder to support additional syntax
    type UpdateBuilder with
        /// Represents monadic computation that returns unit
        /// (e.g. we can now omit 'else' branch in 'if' computation)
        member inline x.Zero() = x.Return(())

        /// Delays a computation with (uncontrolled) side effects
        member inline x.Delay(f) = x.Bind(x.Zero(), f)

        /// Sequential composition of two computations where the
        /// first one has no result (returns a unit value)
        member inline x.Combine(c1, c2) = x.Bind(c1, fun () -> c2)

        /// Enable the 'return!' keyword to return another computation
        member inline x.ReturnFrom(m : UpdateMonad<'S, 'P, 'T>) = m

        /// Ensure that resource 'r' is disposed of at the end of the
        /// computation specified by the function 'f'
        member inline x.Using(r,f) = UM(fun s -> use rr = r in let (UM g) = f rr in g s)

        /// Support 'for' loop - runs body 'f' for each element in 'sq'
        member inline x.For(sq:seq<'V>, f:'V -> UpdateMonad<'S, 'P, unit>) = 
            let rec loop (en:System.Collections.Generic.IEnumerator<_>) = 
                if en.MoveNext() then x.Bind(f en.Current, fun _ -> loop en)
                else x.Zero()
            x.Using(sq.GetEnumerator(), loop)

        /// Supports 'while' loop - run body 'f' until condition 't' holds
        member inline x.While(t, f:unit -> UpdateMonad<'S, 'P, unit>) =
            let rec loop () = 
                if t() then x.Bind(f(), loop)
                else x.Zero()
            loop()
   
    /// Wraps a state of type 'T
    type StateState<'T> = State of 'T

    /// Represents updates on state of type 'T
    type StateUpdate<'T> = 
        | Set of 'T | SetNop
        /// Empty update - do not change the state
        static member Unit = SetNop
        /// Combine updates - return the latest (rightmost) 'Set' update
        static member Combine(a, b) = 
            match a, b with 
            | SetNop, v | v, SetNop -> v 
            | Set a, Set b -> Set b
        /// Apply update to a state - the 'Set' update changes the state
        static member Apply(s, p) = 
            match p with SetNop -> s | Set s -> State s

    /// Set the state to the specified value
    let set s = UM (fun _ -> (Set s,()))
    /// Get the current state 
    let get = UM (fun (State s) -> (SetNop, s))
    /// Run a computation using a specified initial state
    let setRun s (UM f) = f (State s) |> snd