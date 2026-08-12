module Queue exposing
    ( Queue, empty
    , enqueue, dequeue
    , fromList, toList
    )

{-| NOTE: Vendored from turboMaCk/queue 1.1.0

---

Queue is simple FIFO (first in, first out) datastructure.


# Type

@docs Queue, empty


# Query

@docs enqueue, dequeue


# Lists

@docs fromList, toList


# Transformations

-}

-- Types


type alias Rear a =
    List a


type alias Front a =
    List a


{-| Equality checks (`==`) on `Queue` are unreliable due to dynamic distribution of elements.

If you need equality checks use [`toList`](#toList).

    Queue.toList firstQueue == Queue.toList secondQueue

-}
type Queue a
    = Queue (Front a) (Rear a)


{-| private pseudo-constructor
-}
queue : Front a -> Rear a -> Queue a
queue fl rl =
    case fl of
        [] ->
            Queue (List.reverse rl) []

        _ ->
            Queue fl rl


{-| Construct empty `Queue`
-}
empty : Queue a
empty =
    Queue [] []



-- Query


{-| Add item to `Queue`

    Queue.size (Queue.enqueue 1 Queue.empty) == 1

    Queue.size (Queue.enqueue 1 (Queue.fromList [ 1, 2 ])) == 3

-}
enqueue : a -> Queue a -> Queue a
enqueue a (Queue fl rl) =
    queue fl <| a :: rl


{-| Take item from `Queue`

    Queue.dequeue Queue.empty == Nothing

    Queue.dequeue (Queue.fromList [ 1 ]) == Just ( 1, Queue.empty )

-}
dequeue : Queue a -> Maybe ( a, Queue a )
dequeue (Queue fl rl) =
    case fl of
        [] ->
            Nothing

        head :: tail ->
            Just ( head, queue tail rl )



-- Lists


{-| Build `Queue` from `List`

    Queue.fromList [] == Queue.empty

    Queue.size (Queue.fromList [ 1, 2, 3 ]) == 3

-}
fromList : List a -> Queue a
fromList list =
    Queue list []


{-| Convert `Queue` to `List`

    Queue.toList (Queue.fromList []) == []

    Queue.toList (Queue.fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ]

-}
toList : Queue a -> List a
toList (Queue fl rl) =
    fl ++ List.reverse rl



-- Transform
