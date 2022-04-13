module MultiSet

type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>

let empty : MultiSet<'a> = MS Map.empty

let isEmpty (s: MultiSet<'a>) =
    match s with
    | MS s -> Map.isEmpty s
         
let size (s: MultiSet<'a>): uint32 =
    match s with
    | MS s -> Seq.sum (Map.values s)

let contains (k: 'a) (ms: MultiSet<'a>) =
    match ms with
    | MS ms -> Map.containsKey k ms
    
let numItems (a: 'a) (s: MultiSet<'a>) =
    match s with
    | MS s -> s.[a]
    
let add (a: 'a) (n: uint32) (s: MultiSet<'a>) =
    match s with
    | MS s when not (s.ContainsKey a) -> MS (Map.add a n s)
    | MS s                            -> MS (Map.add a (s.[a] + n) s)
    
let addSingle (a: 'a) (s: MultiSet<'a>) = add a (uint32 1) s
    
let remove (a: 'a) (n: uint32) (s: MultiSet<'a>) =
    match s with
    | MS s when not (s.ContainsKey a) -> MS s
    | MS s when s.[a] > n             -> MS (Map.add a (s.[a] - n) s)
    | MS s                            -> MS (Map.remove a s)
        
let removeSingle (a: 'a) (s: MultiSet<'a>) = remove a (uint32 1) s
        
let fold f (s: MultiSet<'a>) acc =
    match s with
    | MS s -> MS (Map.fold f acc s)
    
let foldBack f (s: MultiSet<'a>) acc =
    match s with
    | MS s -> MS (Map.foldBack f s acc)
