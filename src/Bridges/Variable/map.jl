"""
    Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Mapping between bridged variables and the bridge that bridged the variable.
"""
mutable struct Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}
    # Bridged constrained variables
    # `i` ->  `0`: `VariableIndex(-i)` was added with `add_constrained_variable`.
    # `i` -> `-j`: `VariableIndex(-i)` is was the first  variable of
    #              `add_constrained_variables` with a set of dimension `j`.
    # `i` ->  `j`: `VariableIndex(-i)` is was the `j`th  variable of
    #             ` add_constrained_variables`.
    index_in_vector_of_variables::Vector{Int}
    # `i` -> `bridge`: `VariableIndex(-i)` was bridged by `bridge`.
    bridges::Vector{Union{Nothing, AbstractBridge}}
    sets::Vector{Union{Nothing, DataType}}
    # If `nothing`, it cannot be computed because some bridges does not support it
    unbridged_function::Union{Nothing, Dict{MOI.VariableIndex, MOI.AbstractScalarFunction}}
end
Map() = Map(Int[], Union{Nothing, AbstractBridge}[], Union{Nothing, DataType}[], Dict{MOI.VariableIndex, MOI.AbstractScalarFunction}())

# Implementation of `AbstractDict` interface.

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
    if index ≤ 0
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
function Base.delete!(map::Map, vis::Vector{MOI.VariableIndex})
    if has_keys(map, vis)
        map.bridges[bridge_index(map, first(vis))] = nothing
        map.sets[bridge_index(map, first(vis))] = nothing
        return
    else
        throw(ArgumentError("$vis is not a valid key vector as returned by `add_keys_for_bridge`."))
    end
end
function Base.keys(map::Map)
    return MOI.Bridges.LazyFilter(
        vi -> haskey(map, vi),
        MOI.Bridges.LazyMap{MOI.VariableIndex}(
            i -> MOI.VariableIndex(-i),
            eachindex(map.bridges)))
end
Base.length(map::Map) = count(bridge -> bridge !== nothing, map.bridges)
function number_of_variables(map::Map)
    num = 0
    for i in eachindex(map.bridges)
        if map.bridges[i] !== nothing
            if iszero(map.index_in_vector_of_variables[i])
                num += 1
            else
                num += length_of_vector_of_variables(map, MOI.VariableIndex(-i))
            end
        end
        count(bridge -> bridge !== nothing, map.bridges)
    end
    return num
end
function Base.values(map::Map)
    return MOI.Bridges.LazyFilter(bridge -> bridge !== nothing, map.bridges)
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


# Custom interface for information needed by `AbstractBridgeOptimizer`s that is
# not part of the `AbstractDict` interface.

"""
    constrained_set(map::Map, vi::MOI.VariableIndex)

Return the set type in which the bridged variable `vi` was added when it was
bridged.
"""
constrained_set(map::Map, vi::MOI.VariableIndex) = map.sets[bridge_index(map, vi)]

"""
    number_with_set(map::Map, S::Type{<:MOI.AbstractSet})

Return the number of bridged variables in `S`. Note that if `S` is a vector set,
bridging a vector of `n` variables only counts as 1.
"""
function number_with_set(map::Map, S::Type{<:MOI.AbstractSet})
    return count(isequal(S), map.sets)
end

"""
    has_keys(map::Map, vis::Vector{MOI.VariableIndex})::Bool

Return a `Bool` indicating whether `vis` was returned by
[`add_keys_for_bridge`](@ref) and has not been deleted yet.
"""
function has_keys(map::Map, vis::Vector{MOI.VariableIndex})
    return length_of_vector_of_variables(map, first(vis)) == length(vis) &&
        all(vi -> bridge_index(map, vi) == -first(vis).value, vis)
end

"""
    length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)

If `vi` was bridged in a scalar set, it returns 0. Otherwise, it
returns the dimension of the set.
"""
function length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    return -map.index_in_vector_of_variables[bridge_index(map, vi)]
end

"""
    index_in_vector_of_variables(map::Map, vi::MOI.VariableIndex)::IndexInVector

Return the index of `vi` in the vector of variables in which it was bridged.
"""
function index_in_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    index = map.index_in_vector_of_variables[-vi.value]
    if index < 0
        index = 1
    end
    return IndexInVector(index)
end

"""
    has_bridges(map::Map)::Bool

Return a `Bool` indicating whether any bridge were added yet. Note that it
returns `false` even if all bridges were deleted while `isempty` would return
`true`. It is computed in `O(1)` while `isempty` needs `O(n)` hence it is used
by [`MathOptInterface.Bridges.AbstractBridgeOptimizer`](@ref) to shortcut
operations in case variable bridges are not used.
"""
has_bridges(map::Map) = !isempty(map.index_in_vector_of_variables)

"""
    add_key_for_bridge(map::Map, bridge::AbstractBridge,
                       set::MOI.AbstractScalarSet)

Create a new variable index `vi`, stores the mapping `vi => bridge` and
associate `vi` to `typeof(set)`. It returns a tuple with `vi` and the
constraint index
`MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(vi.value)`.
"""
function add_key_for_bridge(map::Map, bridge::AbstractBridge,
                            set::MOI.AbstractScalarSet)
    index = -(length(map.bridges) + 1)
    variable = MOI.VariableIndex(index)
    push!(map.index_in_vector_of_variables, 0)
    push!(map.bridges, bridge)
    push!(map.sets, typeof(set))
    if map.unbridged_function !== nothing
        mappings = unbridged_map(bridge, variable)
        if mappings === nothing
            map.unbridged_function = nothing
        else
            push!(map.unbridged_function, mappings...)
        end
    end
    return variable, MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(index)
end

"""
    add_keys_for_bridge(map::Map, bridge::AbstractBridge,
                        set::MOI.AbstractVectorSet)

Create vector of variable indices `variables`, stores the mapping
`vi => bridge` for each `vi ∈ variables` and associate `variables` to
`typeof(set)`. It returns a tuple with `variables` and the constraint index
`MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(first(variables).value)`.
"""
function add_keys_for_bridge(map::Map, bridge::AbstractBridge,
                             set::MOI.AbstractVectorSet)
    if iszero(MOI.dimension(set))
        return MOI.VariableIndex[], MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(0)
    else
        variables = MOI.VariableIndex[MOI.VariableIndex(-(length(map.bridges) + i))
                                      for i in 1:MOI.dimension(set)]
        push!(map.index_in_vector_of_variables, -MOI.dimension(set))
        push!(map.bridges, bridge)
        push!(map.sets, typeof(set))
        for i in 2:MOI.dimension(set)
            push!(map.index_in_vector_of_variables, i)
            push!(map.bridges, nothing)
            push!(map.sets, nothing)
        end
        if map.unbridged_function !== nothing
            for i in 1:MOI.dimension(set)
                mappings = unbridged_map(bridge, variables[i], IndexInVector(i))
                if mappings === nothing
                    map.unbridged_function = nothing
                else
                    push!(map.unbridged_function, mappings...)
                end
            end
        end
        index = first(variables).value
        return variables, MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(index)
    end
end

"""
    function_for(map::Map, ci::MOI.ConstraintIndex{MOI.SingleVariable})

"""
function function_for(map::Map, ci::MOI.ConstraintIndex{MOI.SingleVariable})
    return MOI.SingleVariable(MOI.VariableIndex(ci.value))
end

"""
    function_for(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables})

"""
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

"""
    unbridged_function(map::Map, vi::MOI.VariableIndex)

"""
function unbridged_function(map::Map, vi::MOI.VariableIndex)
    if map.unbridged_function === nothing
        error("Cannot unbridge function because some variables are bridged by",
              " variable bridges that do not support reverse mapping, e.g.,",
              " `ZerosBridge`.")
    else
        return get(map.unbridged_function, vi, nothing)
    end
end

"""
    EmptyMap <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Empty version of [`Map`](@ref). It is used by
[`MathOptInterface.Bridges.Constraint.SingleBridgeOptimizer`](@ref) as it does
not bridge any variable.
"""
struct EmptyMap <: AbstractDict{MOI.VariableIndex, AbstractBridge} end
Base.isempty(::EmptyMap) = true
function Base.empty!(::EmptyMap) end
Base.length(::EmptyMap) = 0
Base.keys(::EmptyMap) = MOIB.EmptyVector{MOI.VariableIndex}()
Base.values(::EmptyMap) = MOIB.EmptyVector{AbstractBridge}()
has_bridges(::EmptyMap) = false
number_of_variables(::EmptyMap) = 0
number_with_set(::EmptyMap, ::Type{<:MOI.AbstractSet}) = 0
