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
    test_delete_bridged_variable(bridged_mock, vis[2], MOI.LessThan{Float64}, 2, (
        (MOI.VectorOfVariables, MOI.Nonpositives, 0),
    ))
end
