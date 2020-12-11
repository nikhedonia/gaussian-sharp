module GP 
open System

type Vector (N:int, data: array<float>) =
  member this.Item(i:int) = data.[i] 

type Matrix (N:int, M: int, data: array<float>) =
  member this.Data():array<float> = data
  member this.Item(i:int, j:int) = data.[M*i + j] 
  member this.Item(i:int) =
    let vec = 
      seq {
        for j in 0..M do 
          yield this.Item(i, j)
      } 
      |> Seq.toArray
    Vector(M, vec)

  member this.Dims() = (N, M)

  static member (*) (lhs: Matrix, rhs:Matrix) =
    let (Nl, Ml) = lhs.Dims()
    let (Nr, Mr) = rhs.Dims() 
    let S = Math.Min(Ml, Nr)
    let N = Nl
    let M = Mr
    let mutable data = Array.zeroCreate(N*M)

    for i in [0..N-1] do
      for j in [0..M-1] do 
        data.[i*M + j] <- Seq.sumBy (fun k -> lhs.[i, k] * rhs.[k, j]) [0..S-1]
    
    Matrix(N, M, data)

  static member (-) (lhs: Matrix, rhs:Matrix) =
    let (Nl, Ml) = lhs.Dims()
    let (Nr, Mr) = rhs.Dims() 
    let S = Math.Min(Ml, Nr)
    let N = Nl
    let M = Mr
    let mutable data = Array.zeroCreate(N*M)
    
    for i in [0..N-1] do
      for j in [0..M-1] do 
        data.[i*N + j] <- lhs.[i, j] - rhs.[i, j]

    Matrix(N, M, data)

let transposed (m: Matrix) =
  let (N, M) = m.Dims()
  let mutable T = Array.zeroCreate(N*M)
  for i in [0..N-1] do
    for j in [0..M-1] do
      T.[j*N + i] <- m.[i, j]

  Matrix(M, N, T)


let tryCholesky (m: Matrix) =
  match m.Dims() with
  | (N,M) when N = M -> 
    let mutable L = Array.zeroCreate(N*N)

    for i in [0..N-1] do
      for j in [0..i] do
        let s = 
          m.[i, j]
          - Seq.sum(seq{
            for k in [0..j-1] do
              yield L.[i*N + k] * L.[j*N + k]
          })

        L.[i*N+j] <- 
          if i = j
          then Math.Sqrt(s)
          else s / L.[j*N+j]
    Some (Matrix(N, N, L))
  | _ -> None

let invertLowerTriangular (m: Matrix) =
  match m.Dims() with
  | N, M when N = M ->
    let mutable L = Array.zeroCreate(N*N)
    for i in [0..N-1] do
      for j in [0..i] do
        let r = if i = j then 1.0 else 0.0

        let s = r - Seq.sumBy (fun k -> m.[i, k] * L.[M*k + j]) [0..i-1]
        L.[i*N+j] <- s/m.[i,i]

    Some (Matrix(N, N, L))
  | _ -> None 


let invertPositiveDefinite (m: Matrix) =
  let lInv = 
    tryCholesky m
    |> Option.bind invertLowerTriangular
  
  match lInv with
  | Some lInv -> transposed(lInv)*lInv |> Some
  | None -> None
   
let gauss (l:float) (x: float) (y:float) = Math.Exp( -Math.Pow(x - y, 2.0) / l ) 

let covKernel (kernel: float->float->float) (X: seq<float>) (Y: seq<float>) =

  let X = X|>Seq.cache
  let Y = Y|>Seq.cache
  let data =
    seq {
      for x in X do
        for y in Y do
          yield kernel x y 
    }
    |> Seq.toArray
  Matrix(X|>Seq.length, Y|>Seq.length, data)

type Kernel = float -> float -> float
type CovKernel = seq<float> -> seq<float> -> Matrix

let getDiagonal (m: Matrix) =
  let N, M = m.Dims()
  [|0..Math.Min(N, M)-1|] 
  |> Array.map(fun i -> m.[i,i])


let gp (k: Kernel) (X: seq<float>) (Y: seq<float>) =
  let kernel = covKernel k
  let X = X |> Seq.toArray 
  let Y = Y |> Seq.toArray
  let Y = Matrix(Y|>Array.length, 1, Y)
  let kInv = (kernel X X |> invertPositiveDefinite).Value
  let E = (kInv * Y)
  
  let mu (k: Matrix) (_: array<float>) = 
    k * E

  let sigma (k: Matrix) (x: array<float>) = 
    let diag = 
      (kernel x x) - k * kInv * (transposed k)
      |>getDiagonal
      |> Array.map(Math.Abs>>Math.Sqrt)
    diag

  let eval (xx: seq<float>) =
    let xx = xx |> Seq.toArray
    let k = (kernel xx X)

    Seq.zip3
      xx
      ((mu k xx).Data())
      ((sigma k xx))

  eval


let quantile (p:float) (mu:float) (sigma:float) =
  mu + sigma * Math.Sqrt(20.0) * Gaussians.erfinv(2.0*p - 1.0)