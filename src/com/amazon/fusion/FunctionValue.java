// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ArityFailure.Variability;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;
import com.amazon.ion.NullValueException;
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


    //========================================================================
    // Type-checking helpers


    void expectArityExact(int argCount, FusionValue... args)
        throws ArityFailure
    {
        if (args.length != argCount)
        {
            throw new ArityFailure(this, argCount, Variability.EXACT, args);
        }
    }

    void expectArityAtLeast(int atLeast, FusionValue... args)
        throws ArityFailure
    {
        if (args.length < atLeast)
        {
            throw new ArityFailure(this, atLeast, Variability.AT_LEAST, args);
        }
    }


    long assumeLongArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        FusionValue arg = args[argNum];

        try
        {
            DomValue dom = (DomValue) arg;
            IonInt iv = (IonInt) dom.getDom();
            return iv.longValue();
        }
        catch (ClassCastException e)
        {
            throw new ArgTypeFailure(this, "int", argNum, args);
        }
        catch (NullValueException e)
        {
            throw new ArgTypeFailure(this, "int", argNum, args);
        }
    }


    IonValue assumeIonArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        IonValue iv = assumeDomArg(IonValue.class, "Ion datum",
                                   true /* nullable */, argNum, args);
        return iv;
    }


    String assumeTextArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        IonText iv = assumeDomArg(IonText.class, "text", false /* nullable */,
                                  argNum, args);
        return iv.stringValue();
    }


    IonSequence assumeSequenceArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        return assumeDomArg(IonSequence.class, "list or sexp",
                            true /* nullable */, argNum, args);
    }


    IonList assumeListArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        return assumeDomArg(IonList.class, "list", true /* nullable */,
                            argNum, args);
    }


    IonStruct assumeStructArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        return assumeDomArg(IonStruct.class, "struct", true /* nullable */,
                            argNum, args);
    }


    <T extends IonValue> T assumeDomArg(Class<T> klass, String typeName,
                                        boolean nullable,
                                        int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        FusionValue arg = args[argNum];

        try
        {
            DomValue dom = (DomValue) arg;
            IonValue iv = dom.getDom();
            if (nullable || ! iv.isNullValue())
            {
                return klass.cast(iv);
            }
        }
        catch (ClassCastException e)
        {
        }

        throw new ArgTypeFailure(this, typeName, argNum, args);
    }
}
