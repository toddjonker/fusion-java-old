// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.cloneIfContained;
import static com.amazon.fusion.FusionValue.UNDEF;
import com.amazon.ion.IonBool;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import java.util.HashMap;
import java.util.Map;

/**
 * Main entry point to the Fusion evaluation engine.
 */
final class Evaluator
{
    private final IonSystem mySystem;
    private final BaseModule myBaseModule;
    private final Evaluator myOuterFrame;
    private final Map<FusionValue, FusionValue> myContinuationMarks;


    Evaluator(IonSystem system)
    {
        mySystem = system;
        myOuterFrame = null;
        myContinuationMarks = null;
        myBaseModule = new BaseModule(this);
    }

    private Evaluator(IonSystem system, Evaluator outerBindings)
    {
        mySystem = system;
        myBaseModule = outerBindings.myBaseModule;
        myOuterFrame = outerBindings;
        myContinuationMarks = new HashMap<FusionValue, FusionValue>();
    }

    IonSystem getSystem()
    {
        return mySystem;
    }


    //========================================================================

    Namespace newBaseNamespace()
    {
        Namespace base = new Namespace();
        base.use(myBaseModule);
        return base;
    }

    //========================================================================


    FusionValue newBool(boolean value)
    {
        IonBool dom = mySystem.newBool(value);
        return new DomValue(dom);
    }

    FusionValue newInt(long value)
    {
        IonInt dom = mySystem.newInt(value);
        return new DomValue(dom);
    }

    FusionValue newString(String value)
    {
        IonString dom = mySystem.newString(value);
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


    IonValue evalToIon(Environment env, IonValue expr)
        throws FusionException
    {
        FusionValue fv = eval(env, expr);
        return fv.getDom();
    }



    /**
     * @return not null
     */
    FusionValue eval(Environment env, IonValue expr)
        throws FusionException
    {
        while (true)
        {
            switch (expr.getType())
            {
                case LIST:
                {
                    return eval(env, (IonList) expr);
                }
                case STRUCT:
                {
                    return eval(env, (IonStruct) expr);
                }
                case SEXP:
                {
                    FusionValue result = eval(env, (IonSexp) expr);
                    if (result instanceof TailExpression)
                    {
                        TailExpression tail = (TailExpression) result;
                        env = tail.myEnv;
                        expr = tail.myTailExpr;
                        continue;
                    }
                    else if (result == null)
                    {
                        result = UNDEF;
                    }

                    assert ! (result instanceof TailExpression);
                    return result;
                }
                case SYMBOL:
                {
                    return eval(env, (IonSymbol) expr);
                }
                case DATAGRAM:
                {
                    throw new IllegalStateException("Shouldn't have datagram here");
                }
                default:
                {
                    return new DomValue(expr);
                }
            }
        }
    }


    /**
     * @return not null
     */
    private FusionValue eval(Environment env, IonSymbol expr)
        throws FusionException
    {
        String name = expr.stringValue();
        if (name == null)
        {
            throw new SyntaxFailure(null, "null.symbol is not an expression",
                                    expr);
        }

        FusionValue result = env.lookup(name);
        if (result == null)
        {
            throw new UnboundIdentifierFailure(null, expr);
        }
        return result;
    }


    /**
     * @return not null
     */
    private FusionValue eval(Environment env, IonSexp expr)
        throws FusionException
    {
        int len = expr.size();
        if (len == 0)
        {
            throw new SyntaxFailure(null, expr + " is not an expression",
                                    expr);
        }

        IonValue first = expr.get(0);

        FusionValue form = eval(env, first);
        FusionValue result = form.invoke(this, env, expr);
        return result;
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
            IonValue expr = tail.myTailExpr;
            result = eval(env, expr);
        }
        else if (result == null)
        {
            result = UNDEF;
        }
        return result;
    }


    /**
     * Wraps an expression for evaluation in tail position.
     * Must be returned back to this {@link Evaluator} for proper behavior.
     */
    FusionValue bounceTailExpression(Environment env, IonValue tailExpr)
    {
        return new TailExpression(env, tailExpr);
    }


    /**
     * @return not null.
     */
    DomValue eval(Environment env, IonList expr)
        throws FusionException
    {
        IonList resultDom;
        if (expr.isNullValue())
        {
            resultDom = expr;
        }
        else
        {
            ValueFactory vf = expr.getSystem();
            resultDom = vf.newEmptyList();
            for (IonValue elementExpr : expr)
            {
                DomValue elementValue = (DomValue) eval(env, elementExpr);
                IonValue elementDom = elementValue.getDom();
                elementDom = cloneIfContained(elementDom);
                resultDom.add(elementDom);
            }
        }
        return new DomValue(resultDom);
    }


    /**
     * @return not null
     */
    DomValue eval(Environment env, IonStruct expr)
        throws FusionException
    {
        IonStruct resultDom;
        if (expr.isNullValue())
        {
            resultDom = expr;
        }
        else
        {
            ValueFactory vf = expr.getSystem();
            resultDom = vf.newEmptyStruct();
            for (IonValue elementExpr : expr)
            {
                DomValue elementValue = (DomValue) eval(env, elementExpr);
                IonValue elementDom = elementValue.getDom();
                elementDom = cloneIfContained(elementDom);
                resultDom.add(elementExpr.getFieldName(), elementDom);
            }
        }
        return new DomValue(resultDom);
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
        final IonValue myTailExpr;

        TailExpression(Environment env, IonValue tailExpr)
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
}
