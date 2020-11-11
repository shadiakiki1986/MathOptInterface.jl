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

map_function(::Type{<:AbstractToInterval2Bridge{T}}, func) where {T} = MOIU.operate(-, T, func)
# The map is an involution
inverse_map_function(BT::Type{<:AbstractToInterval2Bridge}, func) = map_function(BT, func)
# The map is symmetric
adjoint_map_function(BT::Type{<:AbstractToInterval2Bridge}, func) = map_function(BT, func)
# The map is a symmetric involution
inverse_adjoint_map_function(BT::Type{<:AbstractToInterval2Bridge}, func) = map_function(BT, func)

function MOI.delete(model::MOI.ModelLike, bridge::AbstractToInterval2Bridge, i::IndexInVector)
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOIU.eachscalar(func)[idx]
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    new_set = MOI.update_dimension(set, MOI.dimension(set) - 1)
    MOI.delete(model, bridge.constraint)
    bridge.constraint = MOI.add_constraint(model, new_func, new_set)
end

function MOI.modify(model::MOI.ModelLike, bridge::AbstractToInterval2Bridge,
                    change::MOI.ScalarCoefficientChange)
    MOI.modify(
        model, bridge.constraint,
        MOI.ScalarCoefficientChange(change.variable, -change.new_coefficient))
end
function MOI.modify(model::MOI.ModelLike, bridge::AbstractToInterval2Bridge,
                    change::MOI.MultirowChange{T}) where T
    new_coefficients = Tuple{Int64, T}[
        (index, -coef) for (index, coef) in change.new_coefficients]
    MOI.modify(model, bridge.constraint,
               MOI.MultirowChange(change.variable,
                                  new_coefficients))
end

"""
    GreaterToInterval2Bridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
        AbstractToInterval2Bridge{T, MOI.GreaterThan{T}, MOI.LessThan{T}, F, G}

Transforms a `G`-in-`GreaterThan{T}` constraint into an `F`-in-`LessThan{T}`
constraint.
"""
struct GreaterToInterval2Bridge{T, F<:MOI.AbstractScalarFunction, G<:MOI.AbstractScalarFunction} <:
    AbstractToInterval2Bridge{T, MOI.GreaterThan{T}, MOI.LessThan{T}, F, G}
    constraint::CI{F, MOI.LessThan{T}}
end
map_set(::Type{<:GreaterToInterval2Bridge}, set::MOI.GreaterThan) = MOI.LessThan(-set.lower)
inverse_map_set(::Type{<:GreaterToInterval2Bridge}, set::MOI.LessThan) = MOI.GreaterThan(-set.upper)
function concrete_bridge_type(::Type{<:GreaterToInterval2Bridge{T}},
                              G::Type{<:MOI.AbstractScalarFunction},
                              ::Type{MOI.GreaterThan{T}}) where T
    F = MOIU.promote_operation(-, T, G)
    return GreaterToInterval2Bridge{T, F, G}
end
