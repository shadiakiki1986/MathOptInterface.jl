# The code here is mostly copied from the flip_sign.jl code for FlipSignBridge and GreaterToLessBridge

"""
    AbstractToIntervalBridge{T, S1, S2, F, G}
Bridge a `G`-in-`S1` constraint into an `F`-in-`Interval` constraint by multiplying
the function by `+1` and taking the point reflection of the set across the
origin. The flipped `F`-in-`S` constraint is stored in the `constraint`
field by convention.
"""
abstract type AbstractToIntervalBridge{T, S1<:MOI.AbstractSet} <: SetMapBridge{T, MOI.Interval, S1, MOI.AbstractScalarFunction, MOI.AbstractScalarFunction} end

# The function map is the identity
map_function(::Type{<:AbstractToIntervalBridge{T}}, func) where {T} = MOIU.operate(+, T, func)
# The map is an involution
inverse_map_function(BT::Type{<:AbstractToIntervalBridge}, func) = map_function(BT, func)
# The map is symmetric
adjoint_map_function(BT::Type{<:AbstractToIntervalBridge}, func) = map_function(BT, func)
# The map is a symmetric involution
inverse_adjoint_map_function(BT::Type{<:AbstractToIntervalBridge}, func) = map_function(BT, func)

function MOI.delete(model::MOI.ModelLike, bridge::AbstractToIntervalBridge, i::IndexInVector)
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOIU.eachscalar(func)[idx]
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    new_set = MOI.update_dimension(set, MOI.dimension(set) - 1)
    MOI.delete(model, bridge.constraint)
    bridge.constraint = MOI.add_constraint(model, new_func, new_set)
end

function MOI.modify(model::MOI.ModelLike, bridge::AbstractToIntervalBridge,
                    change::MOI.ScalarCoefficientChange)
    MOI.modify(
        model, bridge.constraint,
        MOI.ScalarCoefficientChange(change.variable, -change.new_coefficient))
end
function MOI.modify(model::MOI.ModelLike, bridge::AbstractToIntervalBridge,
                    change::MOI.MultirowChange{T}) where T
    new_coefficients = Tuple{Int64, T}[
        (index, -coef) for (index, coef) in change.new_coefficients]
    MOI.modify(model, bridge.constraint,
               MOI.MultirowChange(change.variable,
                                  new_coefficients))
end

"""
    GreaterToIntervalBridge{T} <: AbstractToIntervalBridge{T, MOI.GreaterThan{T}}
Transforms a `G`-in-`GreaterThan{T}` constraint into an `F`-in-`Interval{T}`
constraint.
"""
struct GreaterToIntervalBridge{T} <: AbstractToIntervalBridge{T, MOI.GreaterThan{T}}
    constraint::CI{MOI.AbstractScalarFunction, MOI.Interval{T}}
end
map_set(::Type{<:GreaterToIntervalBridge}, set::MOI.GreaterThan) = MOI.Interval(set.lower, Inf)
inverse_map_set(::Type{<:GreaterToIntervalBridge}, set::MOI.Interval) = MOI.GreaterThan(set.lower)
function concrete_bridge_type(::Type{<:GreaterToIntervalBridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.GreaterThan{T}}) where T
    F = MOIU.promote_operation(+, T, MOI.AbstractScalarFunction)
    return GreaterToIntervalBridge{T}
end

"""
    LessToIntervalBridge{T} <: AbstractToIntervalBridge{T, MOI.LessThan{T}}
Transforms a `G`-in-`LessThan{T}` constraint into an `F`-in-`Interval{T}`
constraint.
"""
struct LessToIntervalBridge{T} <: AbstractToIntervalBridge{T, MOI.LessThan{T}}
    constraint::CI{MOI.AbstractScalarFunction, MOI.Interval{T}}
end
map_set(::Type{<:LessToIntervalBridge}, set::MOI.LessThan) = MOI.Interval(-Inf, set.upper)
inverse_map_set(::Type{<:LessToIntervalBridge}, set::MOI.Interval) = MOI.LessThan(set.upper)
function concrete_bridge_type(::Type{<:LessToIntervalBridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.LessThan{T}}) where T
    F = MOIU.promote_operation(+, T, MOI.AbstractScalarFunction)
    return LessToIntervalBridge{T}
end
