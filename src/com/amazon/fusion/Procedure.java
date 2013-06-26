// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionCollection.isCollection;
import static com.amazon.fusion.FusionList.isList;
import static com.amazon.fusion.FusionSequence.isSequence;
import static com.amazon.fusion.FusionStruct.isStruct;
import com.amazon.fusion.BindingDoc.Kind;
import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonText;
import com.amazon.ion.IonTimestamp;
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

    private final BindingDoc myDocs;
    private final int myArity;


    /**
     * @param argNames are used purely for documentation
     */
    Procedure(String doc, String... argNames)
    {
        assert doc == null || ! doc.endsWith("\n");
        myArity = argNames.length;

        StringBuilder buf = new StringBuilder();
        for (String formal : argNames)
        {
            buf.append(' ');
            buf.append(formal);
        }
        String usage = buf.toString();

        myDocs = new BindingDoc(null, Kind.PROCEDURE, usage, doc);
    }


    @Override
    final void nameInferred(String name)
    {
        myDocs.setName(name);
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
    BindingDoc document()
    {
        return myDocs;
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


    void checkArityExact(int arity, Object[] args)
        throws ArityFailure
    {
        if (args.length != arity)
        {
            throw new ArityFailure(this, arity, arity, args);
        }
    }


    /**
     * Checks arity against the documented argument names.
     */
    void checkArityExact(Object[] args)
        throws ArityFailure
    {
        checkArityExact(myArity, args);
    }


    void checkArityAtLeast(int atLeast, Object[] args)
        throws ArityFailure
    {
        if (args.length < atLeast)
        {
            throw new ArityFailure(this, atLeast, Integer.MAX_VALUE, args);
        }
    }


    void checkArityRange(int atLeast, int atMost, Object[] args)
        throws ArityFailure
    {
        if (args.length < atLeast || args.length > atMost)
        {
            throw new ArityFailure(this, atLeast, atMost, args);
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

    /**
     * @return not null.
     */
    BigDecimal checkRequiredDecimalArg(Evaluator eval, int argNum,
                                       Object... args)
        throws ArgTypeFailure
    {
        try
        {
            IonDecimal iv = (IonDecimal) castToIonValueMaybe(args[argNum]);
            BigDecimal result = iv.bigDecimalValue();
            if (result != null)
            {
                return result;
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

    /**
     * @return not null
     */
    String checkStringArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonString iv = checkDomArg(IonString.class, "string",
                                   false /* nullable */, argNum, args);
        return iv.stringValue();
    }


    /**
     * Expects a collection argument, including typed nulls.
     */
    Object checkCollectionArg(Evaluator eval, int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        if (isCollection(eval, arg)) return arg;

        throw argFailure("collection", argNum, args);
    }


    /**
     * Expects a sequence argument, including typed nulls.
     */
    Object checkSequenceArg(Evaluator eval, int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        if (isSequence(eval, arg)) return arg;

        throw argFailure("sequence", argNum, args);
    }

    /**
     * Expects a list argument, including null.list.
     */
    Object checkListArg(Evaluator eval, int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        if (isList(eval, arg)) return arg;

        throw argFailure("list", argNum, args);
    }


    /**
     * Expects a struct argument, including null.struct.
     */
    Object checkStructArg(Evaluator eval, int argNum, Object... args)
        throws ArgTypeFailure
    {
        Object arg = args[argNum];
        if (isStruct(eval, arg)) return arg;

        throw argFailure("struct", argNum, args);
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
        if (isStruct(eval, arg))
        {
            IonValue iv = copyToIonValueMaybe(arg, factory);
            if (iv != null)
            {
                return (IonStruct) iv;
            }
        }

        throw argFailure("ionizable struct", argNum, args);
    }

    /**
     * Allows null.struct.
     * @deprecated Use {@link #copyStructArgToIonStruct}.
     */
    @Deprecated
    IonStruct checkStructArg(int argNum, Object... args)
        throws ArgTypeFailure
    {
        // FIXME ugly compatibility hack!
        if (isStruct(null, args[argNum])) return null;

        throw argFailure("struct", argNum, args);
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
