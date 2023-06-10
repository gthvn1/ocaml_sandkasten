module type X = sig
    val x : int
end

module A = struct
    let x = 0
    let y = 12
end

module IncX = functor (M : X) -> struct
    let x = M.x + 1
end
