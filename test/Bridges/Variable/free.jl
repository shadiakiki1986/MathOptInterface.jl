using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "Free" begin
    bridged_mock = MOIB.Variable.Free{Float64}(mock)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, -100]))
    MOIT.linear6test(bridged_mock, config)

    loc = MOI.get(bridged_mock, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test !((MOI.VectorOfVariables, MOI.Reals) in loc)
    @test (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) in loc
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in loc
    @test MOI.get(mock, MOI.NumberOfVariables()) == 4
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 2
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    @test vis == MOI.VariableIndex.([-1, -2])
    test_delete_bridged_variable(bridged_mock, vis[1], MOI.Reals, 2, (
        (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        (MOI.VectorOfVariables, MOI.Nonpositives, 0)
    ))
    test_delete_bridged_variable(bridged_mock, vis[2], MOI.Reals, 1, (
        (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        (MOI.VectorOfVariables, MOI.Nonpositives, 0)
    ))

    function set_mock_optimize_linear7Test!(mock)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0, 0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, -100]))
    end
    set_mock_optimize_linear7Test!(mock)
    MOIT.linear7test(bridged_mock, config)

    x, y = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    MOI.set(bridged_mock, MOI.VariablePrimalStart(), x, 1.0)
    MOI.set(bridged_mock, MOI.VariablePrimalStart(), y, -1.0)
    xa, xb, ya, yb = MOI.get(mock, MOI.ListOfVariableIndices())
    @test MOI.get(mock, MOI.VariablePrimalStart(), xa) == 1
    @test MOI.get(mock, MOI.VariablePrimalStart(), xb) == 0
    @test MOI.get(mock, MOI.VariablePrimalStart(), ya) == 0
    @test MOI.get(mock, MOI.VariablePrimalStart(), yb) == -1
end
