# Test for NaN returned from callbacks
# https://github.com/jump-dev/MathOptInterface.jl/issues/470

struct NanEvaluator <: MOI.AbstractNLPEvaluator
    nan_objective::Bool
end


# Example from readme of https://github.com/jump-dev/Ipopt.jl#invalid_model-error
MOI.objective_expr(d::NanEvaluator) = :(1 / x[$(VI(1))])

function MOI.constraint_expr(d::NanEvaluator, i::Int)
    if i == 1
        return :(x[$(VI(1))] >= -1.0)
    else
        error("Out of bounds constraint.")
    end
end

MOI.eval_objective(d::NanEvaluator, x) = 1/x[1]

function MOI.eval_constraint(d::NanEvaluator, g, x)
    g[1] = x[1]
end

function MOI.eval_objective_gradient(d::NanEvaluator, grad_f, x)
    grad_f[1] = 1
end

function nancb_template(model::MOI.ModelLike, config::TestConfig, evaluator::NanEvaluator)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supports(model, MOI.NLPBlock())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    lb = [-1.0]
    ub = [Inf]

    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)

    v = MOI.add_variables(model, 1)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    cub = MOI.add_constraint(model, MOI.SingleVariable(v[1]), MOI.LessThan(1.0))
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test cub.value == v[1].value
    clb = MOI.add_constraint(model, MOI.SingleVariable(v[1]), MOI.GreaterThan(-1.0))
    @test clb.value == v[1].value
    MOI.set(model, MOI.VariablePrimalStart(), v[1], 0.9)

    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    # TODO: config.query tests
    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1e-7 atol=atol rtol=rtol

        optimal_v = [1.0]

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ optimal_v atol=atol rtol=rtol

        # TODO: Duals? Maybe better to test on a convex instance.
    end
end

nancb_test(model, config) = nancb_template(model, config, NanEvaluator(true))
nancb_no_hessian_test(model, config) = nancb_template(model, config, NanEvaluator(false))

