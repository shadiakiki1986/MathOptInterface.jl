# Test for NaN returned from callbacks
# https://github.com/jump-dev/MathOptInterface.jl/issues/470

#################
# Maximize  x
#################

function max_x_is_inf(model::MOI.ModelLike, config::TestConfig)
    # based on src/Test/UnitTests/objectives.jl#solve_constant_obj
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: x
        c: x >= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c")
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test c.value == x.value
    test_model_solution(model, config;
        objective_value   = 1.0, #Inf,
        variable_primal   = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c,-1.0)]
    )

end 

###########################
# Maximize x
# Similar to the hs071 test
###########################

# Simple maximization that results in Inf
# max x
# st  x >= 1
# Start at (1,)
# End at (Inf,)

struct MaxXEvaluator <: MOI.AbstractNLPEvaluator
    enable_hessian::Bool
end

function MOI.initialize(d::MaxXEvaluator, requested_features::Vector{Symbol})
    for feat in requested_features
        if !(feat in MOI.features_available(d))
            error("Unsupported feature $feat")
            # TODO: implement Jac-vec and Hess-vec products
            # for solvers that need them
        end
    end
end

function MOI.features_available(d::MaxXEvaluator)
    if d.enable_hessian
        return [:Grad, :Jac, :Hess, :ExprGraph]
    else
        return [:Grad, :Jac, :ExprGraph]
    end
end



# Example from readme of https://github.com/jump-dev/Ipopt.jl#invalid_model-error
MOI.objective_expr(d::MaxXEvaluator) = :(x[$(VI(1))])

function MOI.constraint_expr(d::MaxXEvaluator, i::Int)
    if i == 1
        return :(x[$(VI(1))] >= 1.0)
    else
        error("Out of bounds constraint.")
    end
end

MOI.eval_objective(d::MaxXEvaluator, x) = x[1]

function MOI.eval_constraint(d::MaxXEvaluator, g, x)
    g[1] = x[1]
end

function MOI.eval_objective_gradient(d::MaxXEvaluator, grad_f, x)
    grad_f[1] = 1
end

function MOI.jacobian_structure(d::MaxXEvaluator)
    return Tuple{Int64,Int64}[(1,1)]
end
# lower triangle only
function MOI.hessian_lagrangian_structure(d::MaxXEvaluator)
    @assert d.enable_hessian
    return Tuple{Int64,Int64}[(1,1), (2,1), (2,2)]
end

function MOI.eval_constraint_jacobian(d::MaxXEvaluator, J, x)
    # Constraint (row) 1
    J[1] = 1
end

function MOI.eval_hessian_lagrangian(d::MaxXEvaluator, H, x, σ, μ)
    # Again, only lower left triangle
    # Objective
    H[1] = 0               # 1,1
    H[2] = 0               # 2,1
    H[3] = 0                          # 2,2

    # First constraint
    H[2] += 0  # 2,1
end


function nancb_template(model::MOI.ModelLike, config::TestConfig, evaluator::MaxXEvaluator)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supports(model, MOI.NLPBlock())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    lb = [1.0]
    ub = [Inf]

    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)

    v = MOI.add_variables(model, 1)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    l = 1.0
    start = 1

    clb = MOI.add_constraint(model, MOI.SingleVariable(v[1]), MOI.GreaterThan(l))
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test clb.value == v[1].value
    MOI.set(model, MOI.VariablePrimalStart(), v[1], start)

    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    # TODO: config.query tests
    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.UNKNOWN_RESULT_STATUS

        optimal_v = [4.941613455254625e27] # Inf !?
       
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ optimal_v[1] atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ optimal_v atol=atol rtol=rtol

    end
end

nancb_test(model, config) = nancb_template(model, config, MaxXEvaluator(true))
