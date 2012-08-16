// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

/**
 * Main entry point to the Fusion evaluation engine.
 */
final class Evaluator
{
    private final IonSystem mySystem;
    private final ModuleRegistry myDefaultRegistry;
    private final Evaluator myOuterFrame;
    private final Map<FusionValue, FusionValue> myContinuationMarks;


    Evaluator(IonSystem system, ModuleRegistry defaultRegistry)
    {
        mySystem = system;
        myDefaultRegistry = defaultRegistry;
        myOuterFrame = null;
        myContinuationMarks = null;
    }

    private Evaluator(IonSystem system, Evaluator outerBindings)
    {
        mySystem = system;
        myDefaultRegistry = outerBindings.myDefaultRegistry;
        myOuterFrame = outerBindings;
        myContinuationMarks = new HashMap<FusionValue, FusionValue>();
    }

    IonSystem getSystem()
    {
        return mySystem;
    }

    ModuleRegistry getModuleRegistry()
    {
        return myDefaultRegistry;
    }

    //========================================================================


    KernelModule findKernel()
    {
        return (KernelModule) myDefaultRegistry.lookup(KernelModule.IDENTITY);
    }


    Namespace newModuleNamespace(ModuleRegistry registry)
        throws FusionException
    {
        Namespace ns = new Namespace(registry);
        ns.bind("module", findKernel().getModuleKeyword());
        return ns;
    }


    /**
     * Creates a new namespace sharing a namespace's registry.
     *
     * @param ns carries the {@link ModuleRegistry} to share.
     * Must not be null.
     */
    Namespace newEmptyNamespace(Namespace ns)
    {
        ModuleRegistry registry = ns.getRegistry();
        return new Namespace(registry);
    }


    private Namespace newBaseNamespace(ModuleRegistry registry)
        throws FusionException
    {
        Namespace ns = new Namespace(registry);
        UseKeyword useKeyword = findKernel().getUseKeyword();
        SyntaxValue baseRef = SyntaxSymbol.make("fusion/base");
        useKeyword.use(this, ns, baseRef);
        return ns;
    }


    /**
     * Creates a new namespace sharing this {@link Evaluator}'s default
     * registry and {@code use}ing {@code fusion/base}.
     */
    Namespace newBaseNamespace()
        throws FusionException
    {
        return newBaseNamespace(myDefaultRegistry);
    }


    /**
     * Creates a new namespace sharing a namespace's registry
     * and {@code use}ing {@code fusion/base}.
     *
     * @param ns carries the {@link ModuleRegistry} to share.
     * Must not be null.
     */
    Namespace newBaseNamespace(Namespace ns)
        throws FusionException
    {
        ModuleRegistry registry = ns.getRegistry();
        return newBaseNamespace(registry);
    }

    //========================================================================

    FusionValue newNull(String... annotations)
    {
        IonValue dom = mySystem.newNull();
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newBool(boolean value)
    {
        IonValue dom = mySystem.newBool(value);
        return new DomValue(dom);
    }

    FusionValue newBool(boolean value, String... annotations)
    {
        IonValue dom = mySystem.newBool(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newBool(Boolean value)
    {
        IonValue dom = mySystem.newBool(value);
        return new DomValue(dom);
    }

    FusionValue newBool(Boolean value, String... annotations)
    {
        IonValue dom = mySystem.newBool(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newInt(long value, String... annotations)
    {
        IonValue dom = mySystem.newInt(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newInt(BigInteger value, String... annotations)
    {
        IonValue dom = mySystem.newInt(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newString(String value, String... annotations)
    {
        IonValue dom = mySystem.newString(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newSymbol(String value, String... annotations)
    {
        IonValue dom = mySystem.newSymbol(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newDecimal(BigDecimal value, String... annotations)
    {
        IonValue dom = mySystem.newDecimal(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newDecimal(double value, String... annotations)
    {
        IonValue dom = mySystem.newDecimal(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newFloat(double value)
    {
        IonValue dom = mySystem.newFloat(value);
        return new DomValue(dom);
    }

    FusionValue newFloat(double value, String... annotations)
    {
        IonValue dom = mySystem.newFloat(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newFloat(Double value)
    {
        IonValue dom = (value == null
                           ? mySystem.newNullFloat()
                           : mySystem.newFloat(value));
        return new DomValue(dom);
    }

    FusionValue newFloat(Double value, String... annotations)
    {
        IonValue dom = (value == null
                           ? mySystem.newNullFloat()
                           : mySystem.newFloat(value));
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newTimestamp(Timestamp value, String... annotations)
    {
        IonValue dom = mySystem.newTimestamp(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newBlob(byte[] value, String... annotations)
    {
        IonValue dom = mySystem.newBlob(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    FusionValue newClob(byte[] value, String... annotations)
    {
        IonValue dom = mySystem.newClob(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return new DomValue(dom);
    }

    //========================================================================

    // This is a shady implementation of Racket's continuation marks.
    // It's not full featured: we don't create every continuation frame, so we
    // can't implement the primitive with-continuation-mark.

    FusionValue firstContinuationMark(FusionValue key)
    {
        Evaluator e = this;
        while (e.myOuterFrame != null)
        {
            FusionValue value = e.myContinuationMarks.get(key);
            if (value != null) return value;
            e = e.myOuterFrame;
        }
        return null;
    }


    Evaluator markedContinuation(FusionValue key, FusionValue mark)
    {
        Evaluator innerFrame = new Evaluator(mySystem, this);
        innerFrame.myContinuationMarks.put(key, mark);
        return innerFrame;
    }

    Evaluator markedContinuation(FusionValue[] keys, FusionValue[] marks)
    {
        assert keys.length == marks.length;

        Evaluator innerFrame = new Evaluator(mySystem, this);
        for (int i = 0; i < keys.length; i++)
        {
            innerFrame.myContinuationMarks.put(keys[i], marks[i]);
        }
        return innerFrame;
    }


    //========================================================================


    FusionValue prepareAndEval(Environment env, SyntaxValue source)
        throws FusionException
    {
        source = source.prepare(this, env);
        return eval(env, source);
    }


    SyntaxValue prepare(Environment env, SyntaxValue source)
        throws SyntaxFailure
    {
        return source.prepare(this, env);
    }


    /**
     * @return not null
     */
    FusionValue eval(Environment env, SyntaxValue expr)
        throws FusionException
    {
        moreEval: while (true)
        {
            if (expr.getAnnotations().length != 0)
            {
                String message =
                    "Annotations not supported in raw syntax. You probably " +
                    "want to quote this value";
                throw new SyntaxFailure(null, message, expr);
            }

            FusionValue result = expr.eval(this, env);
            while (true)
            {
                if (result == null)
                {
                    return UNDEF;
                }
                if (result instanceof TailExpression)
                {
                    TailExpression tail = (TailExpression) result;
                    env = tail.myEnv;
                    expr = tail.myTailExpr;
                    continue moreEval;
                }
                if (result instanceof TailCall)
                {
                    TailCall tail = (TailCall) result;
                    result = tail.myProc.invoke(this, tail.myArgs);
                    continue;
                }
                return result;
            }
        }
    }


    /**
     * Makes a <b>non-tail</b> procedure call.
     *
     * @return not null
     */
    FusionValue applyNonTail(Procedure proc, FusionValue... args)
        throws FusionException
    {
        FusionValue result = proc.invoke(this, args);
        if (result instanceof TailExpression)
        {
            TailExpression tail = (TailExpression) result;
            Environment env = tail.myEnv;
            SyntaxValue expr = tail.myTailExpr;
            result = eval(env, expr);
        }
        // TODO handle TailCall, but nothing triggers this path yet.
        else if (result == null)
        {
            result = UNDEF;
        }
        return result;
    }


    //========================================================================


    /**
     * Wraps an expression for evaluation in tail position.
     * Must be returned back to this {@link Evaluator} for proper behavior.
     */
    FusionValue bounceTailExpression(Environment env, SyntaxValue tailExpr)
    {
        return new TailExpression(env, tailExpr);
    }

    /**
     * Wraps an expression for evaluation in tail position.
     * Must be returned back to this {@link Evaluator} for proper behavior.
     *
     * @throws ContractFailure if the given tail isn't a {@link SyntaxValue}.
     */
    FusionValue bounceTailExpression(Environment env, Object tailExpr)
        throws FusionException
    {
        SyntaxValue stx;
        try
        {
            stx = (SyntaxValue) tailExpr;
        }
        catch (ClassCastException e)
        {
            throw new ContractFailure("Expected SyntaxValue, got " +
                                      FusionValue.writeToString(tailExpr));
        }
        return bounceTailExpression(env, stx);
    }


    /**
     * Returned from evaluation of a form when evaluation needs to continue in
     * a tail position. This allows the {@link Evaluator} to trampoline into
     * the tail call without growing the stack.  Not the most efficient
     * implementation, but it works.
     */
    private static final class TailExpression
        extends FusionValue
    {
        final Environment myEnv;
        final SyntaxValue myTailExpr;

        TailExpression(Environment env, SyntaxValue tailExpr)
        {
            myEnv = env;
            myTailExpr = tailExpr;
        }

        @Override
        public void write(Appendable out)
        {
            throw new IllegalStateException();
        }
    }


    /**
     * Makes a procedure call from tail position.
     * The result MUST be immediately returned to the evaluator,
     * it's not a normal value!
     *
     * @return not null
     */
    FusionValue bounceTailCall(Procedure proc, FusionValue... args)
        throws FusionException
    {
        return new TailCall(proc, args);

    }

    /**
     * Returned from evaluation of a form when evaluation needs to continue in
     * a tail position. This allows the {@link Evaluator} to trampoline into
     * the tail call without growing the stack.  Not the most efficient
     * implementation, but it works.
     */
    private static final class TailCall
        extends FusionValue
    {
        final Procedure myProc;
        final FusionValue[] myArgs;

        TailCall(Procedure proc, FusionValue... args)
        {
            myProc = proc;
            myArgs = args;
        }

        @Override
        public void write(Appendable out)
        {
            throw new IllegalStateException();
        }
    }
}
