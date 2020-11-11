"""
    AbstractToInterval2Bridge{T, S1, S2, F, G}

Bridge a `G`-in-`S1` constraint into an `F`-in-`S2` constraint by multiplying
the function by `-1` and taking the point reflection of the set across the
origin. The flipped `F`-in-`S` constraint is stored in the `constraint`
field by convention.
"""
abstract type AbstractToInterval2Bridge{
    T, S1<:MOI.AbstractSet, S2<:MOI.AbstractSet,
    F<:MOI.AbstractFunction, G<:MOI.AbstractFunction} <: SetMapBridge{T, S2, S1, F, G} end

map_function(::Type{<:AbstractToInterval2Bridge{T}}, func) where {T} = func
inverse_map_function(BT::Type{<:AbstractToInterval2Bridge}, func) = func
adjoint_map_function(BT::Type{<:AbstractToInterval2Bridge}, func) = func
inverse_adjoint_map_function(BT::Type{<:AbstractToInterval2Bridge}, func) = func

function MOI.modify(model::MOI.ModelLike, bridge::AbstractToInterval2Bridge,
                    change::MOI.ScalarCoefficientChange)
    MOI.modify(
        model, bridge.constraint,
        MOI.ScalarCoefficientChange(change.variable, change.new_coefficient))
end
function MOI.modify(model::MOI.ModelLike, bridge::AbstractToInterval2Bridge,
                    change::MOI.MultirowChange{T}) where T
    MOI.modify(model, bridge.constraint,
               MOI.MultirowChange(change.variable,
                                  change.new_coefficients))
end

"""
    GreaterToInterval2Bridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
        AbstractToInterval2Bridge{T, MOI.GreaterThan{T}, MOI.Interval{T}, F, G}

Transforms a `G`-in-`GreaterThan{T}` constraint into an `F`-in-`Interval{T}`
constraint.
"""
struct GreaterToInterval2Bridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
    AbstractToInterval2Bridge{T, MOI.GreaterThan{T}, MOI.Interval{T}, F, G}
    constraint::CI{F, MOI.Interval{T}}
end
map_set(::Type{<:GreaterToInterval2Bridge}, set::MOI.GreaterThan) = MOI.Interval(set.lower, Inf)
inverse_map_set(::Type{<:GreaterToInterval2Bridge}, set::MOI.Interval) = MOI.GreaterThan(set.lower)
function concrete_bridge_type(::Type{<:GreaterToInterval2Bridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.GreaterThan{T}}) where T
    return GreaterToInterval2Bridge{T, G, G}
end
