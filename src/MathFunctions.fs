module Gaussians

open System

let erfinv x =
  let mutable p = 0.0
  let mutable w = -Math.Log((1.0-x)*(1.0+x)) // corrected sign
  if w < 5.000000 then
    w <- w - 2.500000
    p <- 2.81022636e-08
    p <- 3.43273939e-07 + p*w
    p <- -3.5233877e-06 + p*w
    p <- -4.39150654e-06 + p*w
    p <- 0.00021858087 + p*w
    p <- -0.00125372503 + p*w
    p <- -0.00417768164 + p*w
    p <- 0.246640727 + p*w
    p <- 1.50140941 + p*w 
  else
    w <- sqrt w - 3.000000
    p <- -0.000200214257
    p <- 0.000100950558 + p*w
    p <- 0.00134934322 + p*w
    p <- -0.00367342844 + p*w
    p <- 0.00573950773 + p*w
    p <- -0.0076224613 + p*w
    p <- 0.00943887047 + p*w
    p <- 1.00167406 + p*w
    p <- 2.83297682 + p*w
  p*x