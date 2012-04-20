// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 * Base class for invocable functions, both built-in and user-defined.
 * This implements the evaluation of arguments and prevents the function from
 * access to the caller's environment.
 */
abstract class FunctionValue
    extends NamedValue
{
    final static String DOTDOTDOT = "...";
    final static String DOTDOTDOTPLUS = "...+";

    final String[] myParams;
    private final String myDoc;

    FunctionValue(String doc, String... params)
    {
        assert doc == null || ! doc.endsWith("\n");
        assert params != null;
        myParams = params;
        myDoc = doc;
    }


    /**
     * Do not call directly! Only for use by the {@link Evaluator} which can
     * properly handle bounced tail expressions.
     *
     * @see Evaluator#applyNonTail(FunctionValue, FusionValue...)
     * @see Evaluator#bounceTailExpression(Environment, IonValue)
     */
    @Override
    final FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        int argCount = expr.size() - 1;

        FusionValue[] args;
        if (argCount == 0)
        {
            args = FusionValue.EMPTY_ARRAY;
        }
        else
        {
            args = new FusionValue[argCount];
            for (int i = 0; i < argCount; i++)
            {
                IonValue argumentExpr = expr.get(i + 1);
                FusionValue argumentValue = eval.eval(env, argumentExpr);
                args[i] = argumentValue;
            }
        }

        return invoke(eval, args);
    }

    final void identify(Appendable out)
        throws IOException
    {
        String name = getInferredName();
        if (name == null)
        {
            out.append("anonymous function");
        }
        else
        {
            out.append("function ");
            IonTextUtils.printQuotedSymbol(out, name);
        }
    }


    @Override
    public final void write(Appendable out)
        throws IOException
    {
        out.append("/* ");
        identify(out);
        out.append(" */");
    }


    @Override
    final void displayHelp(Appendable out)
        throws IOException
    {
        out.append("[FUNCTION]  (");
        out.append(getEffectiveName());
        for (String formal : myParams)
        {
            out.append(' ');
            out.append(formal);
        }
        out.append(")\n");

        if (myDoc != null)
        {
            out.append('\n');
            out.append(myDoc);
            out.append('\n');
        }
    }

    /**
     * @param args must not be null, and none of its elements may be null.
     * @return null is a synonym for {@link #UNDEF}.
     */
    abstract FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException;
}
