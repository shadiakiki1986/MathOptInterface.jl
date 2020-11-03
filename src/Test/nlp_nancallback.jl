# Test for NaN returned from callbacks
# https://github.com/jump-dev/MathOptInterface.jl/issues/470

struct NanEvaluator <: MOI.AbstractNLPEvaluator
    nan_objective::Bool
end

function MOI.initialize(d::NanEvaluator, requested_features::Vector{Symbol})
    for feat in requested_features
        if !(feat in MOI.features_available(d))
            error("Unsupported feature $feat")
            # TODO: implement Jac-vec and Hess-vec products
            # for solvers that need them
        end
    end
end

function MOI.features_available(d::NanEvaluator)
    return [:Grad, :Jac, :Hess, :ExprGraph]
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

function MOI.jacobian_structure(d::NanEvaluator)
    return Tuple{Int64,Int64}[(1,1), (1,2), (1,3), (1,4), (2,1), (2,2),
                              (2,3), (2,4)]
end
# lower triangle only
function MOI.hessian_lagrangian_structure(d::NanEvaluator)
    return Tuple{Int64,Int64}[(1,1), (2,1), (2,2), (3,1), (3,2), (3,3),
                              (4,1), (4,2), (4,3), (4,4)]
end

function MOI.eval_constraint_jacobian(d::NanEvaluator, J, x)
    # Constraint (row) 1
    J[1] = x[2]*x[3]*x[4]  # 1,1
    J[2] = x[1]*x[3]*x[4]  # 1,2
    J[3] = x[1]*x[2]*x[4]  # 1,3
    J[4] = x[1]*x[2]*x[3]  # 1,4
    # Constraint (row) 2
    J[5] = 2*x[1]  # 2,1
    J[6] = 2*x[2]  # 2,2
    J[7] = 2*x[3]  # 2,3
    J[8] = 2*x[4]  # 2,4
end

function MOI.eval_hessian_lagrangian(d::NanEvaluator, H, x, σ, μ)
    # Again, only lower left triangle
    # Objective
    H[1] = σ * (2*x[4])               # 1,1
    H[2] = σ * (  x[4])               # 2,1
    H[3] = 0                          # 2,2
    H[4] = σ * (  x[4])               # 3,1
    H[5] = 0                          # 3,2
    H[6] = 0                          # 3,3
    H[7] = σ* (2*x[1] + x[2] + x[3])  # 4,1
    H[8] = σ * (  x[1])               # 4,2
    H[9] = σ * (  x[1])               # 4,3
    H[10] = 0                         # 4,4

    # First constraint
    H[2] += μ[1] * (x[3] * x[4])  # 2,1
    H[4] += μ[1] * (x[2] * x[4])  # 3,1
    H[5] += μ[1] * (x[1] * x[4])  # 3,2
    H[7] += μ[1] * (x[2] * x[3])  # 4,1
    H[8] += μ[1] * (x[1] * x[3])  # 4,2
    H[9] += μ[1] * (x[1] * x[2])  # 4,3

    # Second constraint
    H[1]  += μ[2] * 2  # 1,1
    H[3]  += μ[2] * 2  # 2,2
    H[6]  += μ[2] * 2  # 3,3
    H[10] += μ[2] * 2  # 4,4

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
