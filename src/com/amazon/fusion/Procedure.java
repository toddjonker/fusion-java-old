// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ArityFailure.Variability;
import com.amazon.ion.IonBool;
import com.amazon.ion.IonContainer;
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
 * Base class for invocable procedures, both built-in and user-defined.
 * This implements the evaluation of arguments and prevents the procedure from
 * access to the caller's environment.
 */
abstract class Procedure
    extends NamedValue
{
    final static String DOTDOTDOT = "...";
    final static String DOTDOTDOTPLUS = "...+";

    final String[] myParams;
    private final String myDoc;

    Procedure(String doc, String... params)
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
     * @see Evaluator#applyNonTail(Procedure, FusionValue...)
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
                try
                {
                    FusionValue argumentValue = eval.eval(env, argumentExpr);
                    args[i] = argumentValue;
                }
                catch (SyntaxFailure e)
                {
                    e.addContext(expr);
                    throw e;
                }
            }
        }

        return invoke(eval, args);
    }

    @Override
    final void identify(Appendable out)
        throws IOException
    {
        String name = getInferredName();
        if (name == null)
        {
            out.append("anonymous procedure");
        }
        else
        {
            out.append("procedure ");
            IonTextUtils.printQuotedSymbol(out, name);
        }
    }


    @Override
    final void displayHelp(Appendable out)
        throws IOException
    {
        out.append("[PROCEDURE]  (");
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


    void checkArityExact(int argCount, FusionValue... args)
        throws ArityFailure
    {
        if (args.length != argCount)
        {
            throw new ArityFailure(this, argCount, Variability.EXACT, args);
        }
    }

    void checkArityAtLeast(int atLeast, FusionValue... args)
        throws ArityFailure
    {
        if (args.length < atLeast)
        {
            throw new ArityFailure(this, atLeast, Variability.AT_LEAST, args);
        }
    }


    final boolean checkBoolArg(int argNum, FusionValue[] args)
        throws ArgTypeFailure
    {
        FusionValue arg = args[argNum];
        try
        {
            DomValue dom = (DomValue) arg;
            IonBool iv = (IonBool) dom.getDom();
            return iv.booleanValue();
        }
        catch (ClassCastException e) {}
        catch (NullValueException e) {}

        throw new ArgTypeFailure(this, "true or false", argNum, args);
    }


    long checkLongArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        FusionValue arg = args[argNum];

        try
        {
            DomValue dom = (DomValue) arg;
            IonInt iv = (IonInt) dom.getDom();
            return iv.longValue();
        }
        catch (ClassCastException e) {}
        catch (NullValueException e) {}

        throw new ArgTypeFailure(this, "int", argNum, args);
    }


    IonValue checkIonArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        IonValue iv = checkDomArg(IonValue.class, "Ion datum",
                                  true /* nullable */, argNum, args);
        return iv;
    }


    /**
     * @return not null
     */
    String checkTextArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        IonText iv = checkDomArg(IonText.class, "text",
                                 false /* nullable */, argNum, args);
        return iv.stringValue();
    }


    IonContainer checkContainerArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonContainer.class, "list, sexp, or struct",
                           true /* nullable */, argNum, args);
    }


    IonSequence checkSequenceArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonSequence.class, "list or sexp",
                           true /* nullable */, argNum, args);
    }


    IonList checkListArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonList.class, "list",
                           true /* nullable */, argNum, args);
    }


    IonStruct checkStructArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonStruct.class, "struct",
                           true /* nullable */, argNum, args);
    }


    <T extends IonValue> T checkDomArg(Class<T> klass, String typeName,
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
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, typeName, argNum, args);
    }

    /** Ensures that an argument is a {@link Procedure}. */
    Procedure checkProcArg(int argNum, FusionValue... args)
        throws ArgTypeFailure
    {
        FusionValue arg = args[argNum];
        try
        {
            return (Procedure) arg;
        }
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, "procedure", argNum, args);
    }
}
