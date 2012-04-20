// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import com.amazon.ion.IonException;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;

/**
 * Main entry point to the Fusion evaluation engine.
 */
final class Evaluator
{
    private final IonSystem mySystem;


    static IonValue cloneIfContained(IonValue value)
    {
        if (value.getContainer() != null)
        {
            value = value.clone();
        }
        return value;
    }

    Evaluator(IonSystem system)
    {
        mySystem = system;
    }


    IonSystem getSystem()
    {
        return mySystem;
    }


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
    FusionValue eval(Environment env, IonSymbol expr)
    {
        String name = expr.stringValue();
        FusionValue result = env.lookup(name);
        if (result == null)
        {
            throw new IonException("No binding for " + name);
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
        if (len < 1) return UNDEF; // TODO throw

        IonValue first = expr.get(0);

        FusionValue form = eval(env, first);
        if (form == null)
        {
            throw new IonException("Bad form: " + first);
        }

        FusionValue result = form.invoke(this, env, expr);
        return result;
    }


    /**
     * Makes a <b>non-tail</b> call to a function.
     *
     * @return not null
     */
    FusionValue applyNonTail(FunctionValue function, FusionValue... args)
        throws FusionException
    {
        FusionValue result = function.invoke(this, args);
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
