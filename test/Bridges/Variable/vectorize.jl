using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "Vectorize" begin
    bridged_mock = MOIB.Variable.Vectorize{Float64}(mock)

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [log(5), 0.0],
                          (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [0.0],
                          (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)   => [[-1.0, log(5)-1, 1/5]])
    MOIT.exp3test(bridged_mock, config)

    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    @test length(vis) == 2

    cis = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone}())
    @test length(cis) == 1

    @testset "MultirowChange" begin
        change = MOI.MultirowChange(vis[2], [(3, 0.0)])
        message = "The change MathOptInterface.MultirowChange{Float64}(MathOptInterface.VariableIndex(-1), Tuple{Int64,Float64}[(3, 0.0)])" *
            " contains variables bridged into a function with nonzero constant."
        err = MOI.ModifyConstraintNotAllowed(cis[1], change, message)
        @test_throws err MOI.modify(bridged_mock, cis[1], change)
    end

    @testset "ScalarCoefficientChange" begin
        change = MOI.ScalarCoefficientChange(vis[2], 0.0)
        attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
        message = "The change MathOptInterface.ScalarCoefficientChange{Float64}(MathOptInterface.VariableIndex(-1), 0.0)" *
            " contains variables bridged into a function with nonzero constant."
        err = MOI.ModifyObjectiveNotAllowed(change, message)
        @test_throws err MOI.modify(bridged_mock, attr, change)
    end

    test_delete_bridged_variable(bridged_mock, vis[2], MOI.LessThan{Float64}, 2, (
        (MOI.VectorOfVariables, MOI.Nonpositives, 0),
    ))
end
