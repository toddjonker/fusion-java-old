// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSequence.isSequence;
import static com.amazon.fusion.FusionSexp.sexpFromIonSequence;
import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.vectorFromIonSequence;
import com.amazon.fusion.ArityFailure.Variability;
import com.amazon.fusion.BindingDoc.Kind;
import com.amazon.ion.IonContainer;
import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonText;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.NullValueException;
import com.amazon.ion.Timestamp;
import com.amazon.ion.ValueFactory;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

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
    BindingDoc document()
    {
        String name = getDocumentedName();

        StringBuilder buf = new StringBuilder();
        buf.append('(');
        buf.append(name);
        for (String formal : getArgNames())
        {
            buf.append(' ');
            buf.append(formal);
        }
        buf.append(')');
        String usage = buf.toString();

        return new BindingDoc(name, Kind.PROCEDURE, usage, myDoc);
    }


    /**
     * Executes a procedure's logic; <b>DO NOT CALL DIRECTLY!</b>
     *
     * @param args must not be null, and none of its elements may be null.
     * @return null is a synonym for {@code void}.
     */
    abstract Object doApply(Evaluator eval, Object[] args)
        throws FusionException;


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


    ArgTypeFailure argFailure(String expectation, int badPos, Object... actuals)
    {
        return new ArgTypeFailure(this, expectation, badPos, actuals);
    }


    <T> T checkArg(Class<T> klass, String desc, int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            T arg = klass.cast(args[argNum]);
            return klass.cast(arg);
        }
        catch (ClassCastException e) {}

        throw new ArgTypeFailure(this, desc, argNum, args);
    }


    double checkDecimalArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonDecimal iv = (IonDecimal) castToIonValueMaybe(args[argNum]);
            return iv.doubleValue();
        }
        catch (ClassCastException e) {}
        catch (NullValueException e) {}
        catch (NullPointerException e) {} // in case toIonValue() ==> null

        throw new ArgTypeFailure(this, "double", argNum, args);
    }


    /**
     * Checks that an argument fits safely into Java's {@code int} type.
     */
    int checkIntArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonInt iv = (IonInt) castToIonValueMaybe(args[argNum]);
            long v = iv.longValue();
            if (Integer.MAX_VALUE < v || v < Integer.MIN_VALUE)
            {
                throw new ArgTypeFailure(this, "32-bit int", argNum, args);
            }
            return (int) v;
        }
        catch (ClassCastException e) {}
        catch (NullValueException e) {}
        catch (NullPointerException e) {} // in case toIonValue() ==> null

        throw new ArgTypeFailure(this, "int", argNum, args);
    }


    long checkLongArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonInt iv = (IonInt) castToIonValueMaybe(args[argNum]);
            // TODO range check
            return iv.longValue();
        }
        catch (ClassCastException e) {}
        catch (NullValueException e) {}
        catch (NullPointerException e) {} // in case toIonValue() ==> null

        throw new ArgTypeFailure(this, "int", argNum, args);
    }

    BigInteger checkBigIntArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonValue dom = FusionValue.castToIonValueMaybe(args[argNum]);

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
        catch (NullPointerException e) {} // in case toIonValue() ==> null

        throw new ArgTypeFailure(this, "int", argNum, args);
    }

    BigDecimal checkBigDecimalArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonDecimal iv = (IonDecimal) castToIonValueMaybe(args[argNum]);
            BigDecimal result = iv.bigDecimalValue();
            if (result != null)
            {
                return iv.bigDecimalValue();
            }
        }
        catch (ClassCastException e) {}
        catch (NullPointerException e) {} // in case toIonValue() ==> null

        throw new ArgTypeFailure(this, "decimal", argNum, args);
    }

    Number checkBigArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonValue ionValue = FusionValue.castToIonValueMaybe(args[argNum]);
            Number result = null;
            if (ionValue instanceof IonInt)
            {
                IonInt iv = (IonInt)ionValue;
                result = iv.bigIntegerValue();
            }
            else if (ionValue instanceof IonDecimal)
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

    IonContainer checkIonContainerArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonContainer.class, "list, sexp, or struct",
                           true /* nullable */, argNum, args);
    }


    /**
     * Expects a sequence argument, coercing {@link IonSequence}s to their
     * equivalent Fusion classes.
     */
    Object coerceSequenceArg(Evaluator eval, int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        if (isSequence(eval, arg)) return arg;

        IonSequence seq;
        try
        {
            seq = (IonSequence) arg;
        }
        catch (ClassCastException e)
        {
            throw argFailure("sequence", argNum, args);
        }

        if (seq.getType() == IonType.LIST)
        {
            return vectorFromIonSequence(eval, seq);
        }

        return sexpFromIonSequence(eval, seq);
    }

    /**
     * Expects a list argument, coercing {@link IonList} to an immutable
     * vector, and {@code null.list} to an empty immutable vector.
     */
    Object coerceListArg(Evaluator eval, int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        if (isVector(eval, arg)) return arg;

        IonList list;
        try
        {
            list = (IonList) arg;
        }
        catch (ClassCastException e)
        {
            throw argFailure("list", argNum, args);
        }

        return vectorFromIonSequence(eval, list);
    }


    /** Allows null.list and vectors. */
    Object checkListArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        if (isVector(arg)) return arg;

        return checkDomArg(IonList.class, "list",
                           true /* nullable */, argNum, args);
    }

    /** Allows null.list */
    IonList checkIonListArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonList.class, "list",
                           true /* nullable */, argNum, args);
    }


    /**
     * Allows {@code null.struct}.
     * @return not null.
     */
    IonStruct copyStructArgToIonStruct(Evaluator eval, ValueFactory factory,
                                       int argNum, Object... args)
        throws FusionException
    {
        Object arg = args[argNum];
        IonStruct is = checkStructArg(argNum, args);
        // Ensure caller can't mutate the "immutable" value.
        return factory.clone(is);
    }

    /**
     * Allows null.struct.
     * @deprecated Use {@link #copyStructArgToIonStruct}.
     */
    @Deprecated
    IonStruct checkStructArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        return checkDomArg(IonStruct.class, "struct",
                           true /* nullable */, argNum, args);
    }


    private <T extends IonValue> T checkDomArg(Class<T> klass, String typeName,
                                               boolean nullable,
                                               int argNum, Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonValue iv = castToIonValueMaybe(args[argNum]);  // TODO copy?!?
            if (iv != null && (nullable || ! iv.isNullValue()))
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
