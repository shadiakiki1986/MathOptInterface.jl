mutable struct Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}
    # Bridged constrained variables
    # `i` -> `0`: `VariableIndex(-i)` was added with `add_constrained_variable`.
    # `i` -> `j`: `VariableIndex(-i)` is was the `j`th  variable of
    #             `add_constrained_variables`.
    index_in_vector_of_variables::Vector{Int}
    # `i` -> `bridge`: `VariableIndex(-i)` was bridged by `bridge`.
    bridges::Vector{Union{Nothing, AbstractBridge}}
    sets::Vector{Union{Nothing, DataType}}
    # If `nothing`, it cannot be computed because some bridges does not support it
    unbridged_function::Union{Nothing, Dict{MOI.VariableIndex, MOI.AbstractScalarFunction}}
end
Map() = Map(Int[], Union{Nothing, AbstractBridge}[], Union{Nothing, DataType}[], Dict{MOI.VariableIndex, MOI.AbstractScalarFunction}())
Base.isempty(map::Map) = all(bridge -> bridge === nothing, map.bridges)
function Base.empty!(map::Map)
    empty!(map.index_in_vector_of_variables)
    empty!(map.bridges)
    empty!(map.sets)
    if map.unbridged_function === nothing
        map.unbridged_function = Dict{MOI.VariableIndex, MOI.AbstractScalarFunction}()
    else
        empty!(map.unbridged_function)
    end
    return map
end
function bridge_index(map::Map, vi::MOI.VariableIndex)
    index = map.index_in_vector_of_variables[-vi.value]
    if iszero(index)
        return -vi.value
    else
        return -vi.value - index + 1
    end
end
function Base.haskey(map::Map, vi::MOI.VariableIndex)
    return -length(map.bridges) ≤ vi.value ≤ -1 &&
        map.bridges[bridge_index(map, vi)] !== nothing
end
function Base.getindex(map::Map, vi::MOI.VariableIndex)
    return map.bridges[bridge_index(map, vi)]
end
function Base.delete!(map::Map, vi::MOI.VariableIndex)
    map.bridges[bridge_index(map, vi)] = nothing
    map.sets[bridge_index(map, vi)] = nothing
    return map
end
function Base.keys(map::Map)
    return MOI.Bridges.LazyFilter(
        vi -> haskey(map, vi),
        MOI.Bridges.LazyMap{MOI.VariableIndex}(
            i -> MOI.VariableIndex(-i),
            eachindex(map.bridges)))
end
Base.length(map::Map) = count(bridge -> bridge !== nothing, map.bridges)
function Base.values(map::Map)
    return MOI.Bridges.LazyFilter(bridge -> bridge !== nothing, map.bridges)
end
constrained_set(map::Map, vi::MOI.VariableIndex) = map.sets[bridge_index(map, vi)]
function number_with_set(map::Map, S::Type{<:MOI.AbstractSet})
    return count(isequal(S), map.sets)
end
function index_in_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    return IndexInVector(map.index_in_vector_of_variables[-vi.value])
end
has_bridges(map::Map) = !isempty(map.index_in_vector_of_variables)
function add_key_for_bridge(map::Map, bridge::AbstractBridge,
                            set::MOI.AbstractScalarSet)
    index = -(length(map.bridges) + 1)
    variable = MOI.VariableIndex(index)
    push!(map.index_in_vector_of_variables, 0)
    push!(map.bridges, bridge)
    push!(map.sets, typeof(set))
    if map.unbridged_function !== nothing
        mapping = unbridged_map(bridge, variable)
        if mapping === nothing
            map.unbridged_function = nothing
        else
            push!(map.unbridged_function, mapping)
        end
    end
    return variable, MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(index)
end
function add_keys_for_bridge(map::Map, bridge::AbstractBridge,
                             set::MOI.AbstractVectorSet)
    if iszero(MOI.dimension(set))
        return MOI.VariableIndex[], MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(0)
    else
        variables = MOI.VariableIndex[MOI.VariableIndex(-(length(map.bridges) + i))
                                      for i in 1:MOI.dimension(set)]
        push!(map.index_in_vector_of_variables, 1)
        push!(map.bridges, bridge)
        push!(map.sets, typeof(set))
        for i in 2:MOI.dimension(set)
            push!(map.index_in_vector_of_variables, i)
            push!(map.bridges, nothing)
            push!(map.sets, nothing)
        end
        if map.unbridged_function !== nothing
            for i in 1:MOI.dimension(set)
                mapping = unbridged_map(bridge, variables[i], IndexInVector(i))
                if mapping === nothing
                    map.unbridged_function = nothing
                else
                    push!(map.unbridged_function, mapping)
                end
            end
        end
        index = first(variables).value
        return variables, MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(index)
    end
end
function function_for(map::Map, ci::MOI.ConstraintIndex{MOI.SingleVariable})
    return MOI.SingleVariable(MOI.VariableIndex(ci.value))
end
function function_for(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables})
    variables = MOI.VariableIndex[]
    for i in ci.value:-1:-length(map.bridges)
        vi = MOI.VariableIndex(i)
        if bridge_index(map, vi) == -ci.value
            push!(variables, vi)
        else
            break
        end
    end
    return MOI.VectorOfVariables(variables)
end
function Base.iterate(map::Map, state=1)
    while state ≤ length(map.bridges) && map.bridges[state] === nothing
        state += 1
    end
    if state > length(map.bridges)
        return nothing
    else
        return MOI.VariableIndex(-state) => map.bridges[state], state + 1
    end
end
function unbridged_function(map::Map, vi::MOI.VariableIndex)
    if map.unbridged_function === nothing
        error("Cannot unbridge function because some variables are bridged by",
              " variable bridges that do not support reverse mapping, e.g.,",
              " `ZerosBridge`.")
    else
        return get(map.unbridged_function, vi, MOI.SingleVariable(vi))
    end
end

struct EmptyMap <: AbstractDict{MOI.VariableIndex, AbstractBridge} end
Base.isempty(::EmptyMap) = true
function Base.empty!(::EmptyMap) end
Base.length(::EmptyMap) = 0
Base.keys(::EmptyMap) = MOIB.EmptyVector{MOI.VariableIndex}()
Base.values(::EmptyMap) = MOIB.EmptyVector{AbstractBridge}()
has_bridges(::EmptyMap) = false
number_with_set(::EmptyMap, ::Type{<:MOI.AbstractSet}) = 0

"""
    SingleBridgeOptimizer{BT<:AbstractBridge, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `SingleBridgeOptimizer` bridges any constrained variables supported by the
bridge `BT`. This is in contrast with the [`MathOptInterface.Bridges.LazyBridgeOptimizer`](@ref)
which only bridges the constrained variables that are unsupported by the internal model,
even if they are supported by one of its bridges.
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge, OT<:MOI.ModelLike} <: MOIB.AbstractBridgeOptimizer
    model::OT
    map::Map # index of bridged variable -> variable bridge
end
function SingleBridgeOptimizer{BT}(model::OT) where {BT, OT <: MOI.ModelLike}
    SingleBridgeOptimizer{BT, OT}(model, Map())
end

function bridges(bridge::MOI.Bridges.AbstractBridgeOptimizer)
    return EmptyMap()
end
bridges(bridge::SingleBridgeOptimizer) = bridge.map

function MOIB.supports_bridging_constraint(
    b::SingleBridgeOptimizer{BT}, ::Type{MOI.SingleVariable},
    S::Type{<:MOI.AbstractScalarSet}) where BT
    return supports_constrained_variables(BT, S)
end
function MOIB.supports_bridging_constraint(
    b::SingleBridgeOptimizer{BT}, ::Type{MOI.VectorOfVariables},
    S::Type{<:MOI.AbstractVectorSet}) where BT
    return supports_constrained_variables(BT, S)
end
function MOIB.is_bridged(b::SingleBridgeOptimizer, S::Type{<:MOI.AbstractScalarSet})
    return MOIB.supports_bridging_constraint(b, MOI.SingleVariable, S)
end
function MOIB.is_bridged(b::SingleBridgeOptimizer, S::Type{<:MOI.AbstractVectorSet})
    return MOIB.supports_bridging_constraint(b, MOI.VectorOfVariables, S)
end
function MOIB.is_bridged(::SingleBridgeOptimizer,
                         ::Type{<:MOI.AbstractFunction},
                         ::Type{<:MOI.AbstractSet})
    return false
end
function MOIB.bridge_type(::SingleBridgeOptimizer{BT},
                          ::Type{<:MOI.AbstractSet}) where BT
    return BT
end