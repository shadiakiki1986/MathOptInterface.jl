using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

const SAF = MOI.ScalarAffineFunction{Float64}
const GT = MOI.GreaterThan{Float64}

@testset "GreaterToInterval2" begin
    bridged_mock = MOIB.Constraint.GreaterToInterval2{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [
                             #MOI.SingleVariable,
                             MOI.ScalarAffineFunction{Float64},
                             MOI.ScalarQuadraticFunction{Float64}]
                   for S in [MOI.GreaterThan{Float64}]])

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100.0, -100.0]))
    MOIT.linear6test(bridged_mock, config)

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()))

    @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) ≈ 2.0
    end

    test_delete_bridge(bridged_mock, ci, 2,
                       ((MOI.ScalarAffineFunction{Float64},
                         MOI.Interval{Float64}, 0),))
end
