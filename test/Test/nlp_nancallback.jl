# This file is very similar to test/Test/nlp.jl

using Test

import MathOptInterface
const MOI = MathOptInterface

const MOIT = MOI.Test
const MOIU = MOI.Utilities


@testset "max_x_is_inf" begin
    # `UniversalFallback` needed for `MOI.Silent`
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    # Optimizers attributes have to be set to default value since the mock
    # optimizer doesn't handle this
    MOI.set(mock, MOI.Silent(), true)
    MOI.set(mock, MOI.TimeLimitSec(), nothing)
    MOI.set(mock, MOI.NumberOfThreads(), nothing)
    config = MOIT.TestConfig()

    @testset "max_x_is_inf" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1]),
                MOI.FEASIBLE_POINT
            )
        )
        MOIT.max_x_is_inf(mock, config)
    end

end

@testset "max_inv_x_is_inf" begin
    mock = MOIU.MockOptimizer(
        MOIU.UniversalFallback(MOIU.Model{Float64}()),
        eval_objective_value=false
    )
    config = MOIT.TestConfig(optimal_status = MOI.LOCALLY_SOLVED)
    MOIU.set_mock_optimize!(
        mock,
        (mock) -> begin
            MOIU.mock_optimize!(
                mock, config.optimal_status,
                [1.0]
            )
            MOI.set(mock, MOI.ObjectiveValue(), 1e-7)
        end
    )
    MOIT.nancb_test(mock, config)

    d = MOIT.NanEvaluator(false)
    VI = MOI.VariableIndex
    @test MOI.objective_expr(d) == :(1 / x[$(VI(1))])
    @test MOI.constraint_expr(d, 1) ==
        :(x[$(VI(1))] >= -1.0)
    @test_throws ErrorException MOI.constraint_expr(d, 2)
end
