# These tests are mostly copies of the flip_sign.jl tests for GreaterToLess

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

@testset "GreaterToInterval" begin
    bridged_mock = MOIB.Constraint.GreaterToInterval{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.ScalarAffineFunction{Float64},
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

    println(">>>>")
    println("pre test del")
    print(ci)
    println("")
    print(MOIB.added_constraint_types(typeof(MOIB.bridge(bridged_mock, ci))))
    println("")
    bridged_2 = MOIB.LazyBridgeOptimizer(mock)
    println(sprint(MOIB.print_graph, bridged_2))
    println("<<<<")

    test_delete_bridge(bridged_mock, ci, 2,
                       ((MOI.ScalarAffineFunction{Float64},
                         MOI.Interval{Float64}, 1),
                       ))
end


@testset "LessToInterval" begin
    bridged_mock = MOIB.Constraint.LessToInterval{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.ScalarAffineFunction{Float64},
                             MOI.ScalarQuadraticFunction{Float64}]
                   for S in [MOI.LessThan{Float64}]])

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100.0, -100.0]))
    MOIT.linear6test(bridged_mock, config)

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}()))

    @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) ≈ 2.0
    end

    test_delete_bridge(bridged_mock, ci, 2,
                       ((MOI.ScalarAffineFunction{Float64},
                         MOI.Interval{Float64}, 1),))
end


@testset "GreaterOrLessToInterval-unmocked" begin
    """
    Dummy optimizer that supports Interval only
    """
    module OnlyIntervalOptimizer
      using MathOptInterface
      const MOI  = MathOptInterface
      
      mutable struct Optimizer <: MOI.AbstractOptimizer
        function Optimizer()
          return new()
        end
      end
        
      MOI.get(model::Optimizer, ::MOI.SolverName) = "OnlyIntervalOptimizer"

      MOI.supports_constraint(::Optimizer, ::Type{MOI.ScalarAffineFunction{Float64}}, ::Type{MOI.Interval{Float64}}) = true
    end

    # model supports Interval but not LessThan or GreaterThan
    model = OnlyIntervalOptimizer.Optimizer()
    @test  MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
    @test !MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test !MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})

    # bridged model supports all
    bridged = GreaterToInterval{Float64}(LessToInterval{Float64}(model))
    @test  MOI.supports_constraint(bridged, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
    @test  MOI.supports_constraint(bridged, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test  MOI.supports_constraint(bridged, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
end
