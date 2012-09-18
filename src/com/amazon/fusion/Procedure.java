// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ArityFailure.Variability;
import com.amazon.ion.IonContainer;
import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonText;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;
import com.amazon.ion.NullValueException;
import com.amazon.ion.Timestamp;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

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

    private final String[] myArgNames;
    private final String myDoc;

    /**
     * @param argNames are used purely for documentation
     */
    Procedure(String doc, String... argNames)
    {
        assert doc == null || ! doc.endsWith("\n");
        assert argNames != null;
        myArgNames = argNames;
        myDoc = doc;
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


    /**
     * Allows subclass to override and compute param names on-demand.
     * @return not null.
     */
    String[] getArgNames()
    {
        return myArgNames;
    }


    @Override
    final void displayHelp(Appendable out)
        throws IOException
    {
        out.append("[PROCEDURE]  (");
        out.append(getEffectiveName());
        for (String formal : getArgNames())
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
     *
     * @deprecated Subclasses should implement
     * {@link #doApply(Evaluator, Object[])} instead
     */
    @Deprecated
    Object invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        return doApply(eval, args);
    }


    /**
     * Executes a procedure's logic; <b>DO NOT CALL DIRECTLY!</b>
     *
     * @param args must not be null, and none of its elements may be null.
     * @return null is a synonym for {@link #UNDEF}.
     */
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        FusionValue[] fArgs =
            Arrays.copyOf(args, args.length, FusionValue[].class);
        return invoke(eval, fArgs);
    }


    //========================================================================
    // Type-checking helpers


    void checkArityExact(int argCount, Object[] args)
        throws ArityFailure
    {
        if (args.length != argCount)
        {
            throw new ArityFailure(this, argCount, Variability.EXACT, args);
        }
    }


    /**
     * Checks arity against the documented argument names.
     */
    void checkArityExact(Object[] args)
        throws ArityFailure
    {
        checkArityExact(myArgNames.length, args);
    }


    void checkArityAtLeast(int atLeast, Object[] args)
        throws ArityFailure
    {
        if (args.length < atLeast)
        {
            throw new ArityFailure(this, atLeast, Variability.AT_LEAST, args);
        }
    }


    final boolean checkBoolArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        return checkBoolArg(argNum, arg);
    }

    double checkDecimalArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            DomValue dom = (DomValue) args[argNum];
            IonDecimal iv = (IonDecimal) dom.ionValue();
            return iv.doubleValue();
        }
        catch (ClassCastException e) {}
        catch (NullValueException e) {}
        catch (NullPointerException e) {}

        throw new ArgTypeFailure(this, "double", argNum, args);
    }


    long checkLongArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            DomValue dom = (DomValue) args[argNum];
            IonInt iv = (IonInt) dom.ionValue();
            return iv.longValue();
        }
        catch (ClassCastException e) {}
        catch (NullValueException e) {}
        catch (NullPointerException e) {}

        throw new ArgTypeFailure(this, "int", argNum, args);
    }

    BigInteger checkBigIntArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonValue dom = FusionValue.toIonValue(args[argNum]);

        try
        {
            IonInt iv = (IonInt)dom;
            BigInteger result = iv.bigIntegerValue();
            if (result != null)
            {
                return result;
            }
        }
        catch (ClassCastException e)   {}
        catch (NullPointerException e) {}

        throw new ArgTypeFailure(this, "int", argNum, args);
    }

    BigDecimal checkBigDecimalArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            DomValue dom = (DomValue) args[argNum];
            IonDecimal iv = (IonDecimal)dom.ionValue();
            BigDecimal result = iv.bigDecimalValue();
            if (result != null)
            {
                return iv.bigDecimalValue();
            }
        }
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, "decimal", argNum, args);
    }

    Number checkBigArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonValue ionValue = FusionValue.toIonValue(args[argNum]);
            Number result = null;
            if (ionValue instanceof IonInt)
            {
                IonInt iv = (IonInt)ionValue;
                result = iv.bigIntegerValue();
            } else if (ionValue instanceof IonDecimal)
            {
                IonDecimal iv = (IonDecimal)ionValue;
                result = iv.bigDecimalValue();
            }

            if (result != null)
            {
                return result;
            }
        }
        catch (ClassCastException e) {}
        catch (NullPointerException e) {} // in case toIonValue() ==> null

        throw new ArgTypeFailure(this, "int or decimal", argNum, args);
    }

    Timestamp checkTimestampArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonTimestamp iv = checkDomArg(IonTimestamp.class, "timestamp",
                                  true /* nullable */, argNum, args);
        return iv.timestampValue();
    }


    IonValue checkIonArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonValue iv = checkDomArg(IonValue.class, "Ion datum",
                                  true /* nullable */, argNum, args);
        return iv;
    }


    /**
     * @return not null
     */
    String checkTextArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonText iv = checkDomArg(IonText.class, "text",
                                 false /* nullable */, argNum, args);
        return iv.stringValue();
    }

    String checkStringArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonString iv = checkDomArg(IonString.class, "text",
                                   false /* nullable */, argNum, args);
        return iv.stringValue();
    }

    IonContainer checkContainerArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonContainer.class, "list, sexp, or struct",
                           true /* nullable */, argNum, args);
    }


    IonSequence checkSequenceArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonSequence.class, "list or sexp",
                           true /* nullable */, argNum, args);
    }


    IonList checkListArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonList.class, "list",
                           true /* nullable */, argNum, args);
    }


    IonStruct checkStructArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonStruct.class, "struct",
                           true /* nullable */, argNum, args);
    }

    Stream checkStreamArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            return (Stream) args[argNum];
        }
        catch (ClassCastException e) { }

        throw new ArgTypeFailure(this, "stream", argNum, args);
    }

    <T extends IonValue> T checkDomArg(Class<T> klass, String typeName,
                                       boolean nullable,
                                       int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            DomValue dom = (DomValue) args[argNum];
            IonValue iv = dom.ionValue();
            if (nullable || ! iv.isNullValue())
            {
                return klass.cast(iv);
            }
        }
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, typeName, argNum, args);
    }


    SyntaxValue checkSyntaxArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            return (SyntaxValue) args[argNum];
        }
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, "Syntax value", argNum, args);
    }

    <T extends SyntaxValue> T checkSyntaxArg(Class<T> klass, String typeName,
                                             boolean nullable,
                                             int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];

        try
        {
            SyntaxValue stx = (SyntaxValue) arg;
            if (nullable || ! stx.isNullValue())
            {
                return klass.cast(stx);
            }
        }
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, typeName, argNum, args);
    }


    SyntaxSymbol checkSyntaxSymbolArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkSyntaxArg(SyntaxSymbol.class, "syntax symbol",
                              true /* nullable */, argNum, args);
    }


    SyntaxContainer checkSyntaxContainerArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkSyntaxArg(SyntaxContainer.class,
                              "syntax_list, sexp, or struct",
                              true /* nullable */, argNum, args);
    }



    SyntaxSequence checkSyntaxSequenceArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkSyntaxArg(SyntaxSequence.class,
                              "syntax_list or syntax_sexp",
                              true /* nullable */, argNum, args);
    }

    /** Ensures that an argument is a {@link Procedure}. */
    Procedure checkProcArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            return (Procedure) args[argNum];
        }
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, "procedure", argNum, args);
    }

}
