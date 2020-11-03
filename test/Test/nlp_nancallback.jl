using Test

import MathOptInterface
const MOI = MathOptInterface

@testset "nancallback" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        eval_objective_value=false
    )
    config = MOI.Test.TestConfig(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock, config.optimal_status,
                [1.0]
            )
            MOI.set(mock, MOI.ObjectiveValue(), 1e-7)
        end
    )
    MOI.Test.nancb_test(mock, config)
    MOI.Test.nancb_no_hessian_test(mock, config)

    d = MOI.Test.NanEvaluator(false)
    VI = MOI.VariableIndex
    @test MOI.objective_expr(d) == :(1 / x[$(VI(1))])
    @test MOI.constraint_expr(d, 1) ==
        :(x[$(VI(1))] >= -1.0)
    @test_throws ErrorException MOI.constraint_expr(d, 2)
end

