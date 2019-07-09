using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

bridged_mock = MOIB.Variable.Vectorize{Float64}(mock)

mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [log(5), 0.0],
                      (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [0.0],
                      (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)   => [[-1.0, log(5)-1, 1/5]])
MOIT.exp3test(bridged_mock, config)

vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
@test length(vis) == 2
y = vis[2]

cis = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{
    MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone}())
@test length(cis) == 1

@testset "get `UnknownVariableAttribute``" begin
    err = ArgumentError(
        "Variable bridge of type `MathOptInterface.Bridges.Variable.VectorizeBridge{Float64,MathOptInterface.Nonpositives}`" *
        " does not support accessing the attribute `MathOptInterface.Test.UnknownVariableAttribute()`."
    )
    @test_throws err MOI.get(bridged_mock, MOIT.UnknownVariableAttribute(), y)
end

@testset "set `ConstraintSet`" begin
    ci = MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}(y.value)
    attr = MOI.ConstraintSet()
    err = MOI.SetAttributeNotAllowed(attr,
        "The variable MathOptInterface.VariableIndex(12345676) is bridged by the `VectorizeBridge`.")
    @test_throws err MOI.set(bridged_mock, attr, ci, MOI.LessThan(4.0))
end

@testset "MultirowChange" begin
    change = MOI.MultirowChange(y, [(3, 0.0)])
    message = "The change MathOptInterface.MultirowChange{Float64}(MathOptInterface.VariableIndex(-1), Tuple{Int64,Float64}[(3, 0.0)])" *
        " contains variables bridged into a function with nonzero constant."
    err = MOI.ModifyConstraintNotAllowed(cis[1], change, message)
    @test_throws err MOI.modify(bridged_mock, cis[1], change)
end

@testset "ScalarCoefficientChange" begin
    change = MOI.ScalarCoefficientChange(y, 0.0)
    attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    message = "The change MathOptInterface.ScalarCoefficientChange{Float64}(MathOptInterface.VariableIndex(-1), 0.0)" *
        " contains variables bridged into a function with nonzero constant."
    err = MOI.ModifyObjectiveNotAllowed(change, message)
    @test_throws err MOI.modify(bridged_mock, attr, change)
end

test_delete_bridged_variable(bridged_mock, y, MOI.LessThan{Float64}, 2, (
    (MOI.VectorOfVariables, MOI.Nonpositives, 0),
))
