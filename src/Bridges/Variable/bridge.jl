"""
    AbstractBridge

Subtype of [`MathOptInterface.Bridges.AbstractBridge`](@ref) for variable
bridges.
"""
abstract type AbstractBridge <: MOIB.AbstractBridge end

"""
    IndexInVector

Index of variable in vector of variables.
"""
struct IndexInVector
    value::Int
end

"""
    bridge_constrained_variable(BT::Type{<:AbstractBridge}, model::MOI.ModelLike,
                                set::MOI.AbstractSet)

Bridge the constrained variable in `set` using bridge `BT` to `model` and returns
a bridge object of type `BT`. The bridge type `BT` should be a concrete type,
that is, all the type parameters of the bridge should be set. Use
[`concrete_bridge_type`](@ref) to obtain a concrete type for given set types.
"""
function bridge_constrained_variable end

function MOI.get(::MOI.ModelLike, attr::MOI.AbstractVariableAttribute,
                 bridge::AbstractBridge, ::IndexInVector)
    throw(ArgumentError("Variable bridge of type `$(typeof(bridge))` does not support accessing the attribute `$attr`."))
end

"""
    supports_constrained_variables(::Type{<:AbstractBridge},
                                   ::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging
constrained variables in `S`.
"""
function supports_constrained_variables(::Type{<:AbstractBridge},
                                        ::Type{<:MOI.AbstractSet})
    return false
end

"""
    added_constrained_variable_types(BT::Type{<:AbstractBridge},
                                     S::Type{<:MOI.AbstractSet})

Return a list of the types of constraints that bridges of type `BT` add for
bridging constrained variabled in `S`.
"""
function MOIB.added_constrained_variable_types(BT::Type{<:AbstractBridge},
                                     S::Type{<:MOI.AbstractSet})
    MOIB.added_constrained_variable_types(concrete_bridge_type(BT, S))
end

"""
    added_constraint_types(BT::Type{<:AbstractBridge},
                                     S::Type{<:MOI.AbstractSet})

Return a list of the types of constrained variables that bridges of type `BT` add
for bridging constrained variabled in `S`.
"""
function MOIB.added_constraint_types(BT::Type{<:AbstractBridge},
                                     S::Type{<:MOI.AbstractSet})
    MOIB.added_constraint_types(concrete_bridge_type(BT, S))
end

function concrete_bridge_type(bridge_type::DataType,
                              ::Type{<:MOI.AbstractSet})
    return bridge_type
end

function concrete_bridge_type(b::MOIB.AbstractBridgeOptimizer,
                              S::Type{<:MOI.AbstractSet})
    return concrete_bridge_type(MOIB.bridge_type(b, S), S)
end
