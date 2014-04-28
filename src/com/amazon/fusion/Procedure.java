// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionCollection.checkNullableCollectionArg;
import static com.amazon.fusion.FusionList.checkNullableListArg;
import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;
import static com.amazon.fusion.FusionNumber.checkIntArgToJavaLong;
import static com.amazon.fusion.FusionNumber.checkNullableIntArg;
import static com.amazon.fusion.FusionNumber.checkRequiredIntArg;
import static com.amazon.fusion.FusionSequence.checkNullableSequenceArg;
import static com.amazon.fusion.FusionStruct.checkNullableStructArg;
import com.amazon.fusion.BindingDoc.Kind;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
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


    /**
     * Creates, but does not throw, an exception that indicates a contract
     * failure with a given argument.
     *
     * @param expectation describes the expectation that was not met.  When
     * displayed, this string is prefixed by "procedure <i>p</i> expects",
     * so the content should be a noun phrase.
     * @param badPos the zero-based index of the problematic value.
     * -1 means a specific position isn't implicated.
     * @param actuals the provided procedure arguments;
     * must not be null or zero-length.
     *
     * @return a new exception.
     */
    ArgumentException argFailure(String expectation, int badPos, Object... actuals)
    {
        return new ArgumentException(this, expectation, badPos, actuals);
    }


    <T> T checkArg(Class<T> klass, String desc, int argNum, Object... args)
        throws ArgumentException
    {
        try
        {
            T arg = klass.cast(args[argNum]);
            return klass.cast(arg);
        }
        catch (ClassCastException e) {}

        throw new ArgumentException(this, desc, argNum, args);
    }



    /**
     * Checks that an argument fits safely into Java's {@code int} type.
     *
     * @deprecated Use helpers in {@link FusionNumber}.
     */
    @Deprecated
    int checkIntArg(int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkIntArgToJavaInt(/*eval*/ null,           // NOT SUPPORTED!
                                    this, argNum, args);
    }


    /**
     * Checks that an argument fits safely into Java's {@code long} type.
     *
     * @deprecated Use helpers in {@link FusionNumber}.
     */
    @Deprecated
    long checkLongArg(int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkIntArgToJavaLong(/*eval*/ null,          // NOT SUPPORTED!
                                     this, argNum, args);
    }


    /**
     * @return not null.
     *
     * @deprecated Use helpers in {@link FusionNumber}.
     */
    @Deprecated
    BigInteger checkBigIntArg(int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkRequiredIntArg(/*eval*/ null,            // NOT SUPPORTED!
                                   this, argNum, args);
    }

    /**
     * @return may be null.
     *
     * @deprecated Use helpers in {@link FusionNumber}.
     */
    @Deprecated
    BigInteger checkBigIntArg(Evaluator eval, int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkNullableIntArg(eval, this, argNum, args);
    }


    /**
     * Expects a collection argument, including typed nulls.
     *
     * @deprecated Use
     * {@link FusionCollection#checkNullableCollectionArg(Evaluator, Procedure, int, Object...)}
     */
    @Deprecated
    Object checkCollectionArg(Evaluator eval, int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkNullableCollectionArg(eval, this, argNum, args);
    }


    /**
     * Expects a sequence argument, including typed nulls.
     *
     * @deprecated Use
     * {@link FusionSequence#checkNullableSequenceArg(Evaluator, Procedure, int, Object...)}
     */
    @Deprecated
    Object checkSequenceArg(Evaluator eval, int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkNullableSequenceArg(eval, this, argNum, args);
    }

    /**
     * Expects a list argument, including null.list.
     *
     * @deprecated Use
     * {@link FusionList#checkNullableListArg(Evaluator, Procedure, int, Object...)}
     */
    @Deprecated
    Object checkListArg(Evaluator eval, int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkNullableListArg(eval, this, argNum, args);
    }


    /**
     * Expects a struct argument, including null.struct.
     *
     * @deprecated Use
     * {@link FusionStruct#checkNullableStructArg(Evaluator, Procedure, int, Object...)}
     */
    @Deprecated
    Object checkStructArg(Evaluator eval, int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkNullableStructArg(eval, this, argNum, args);
    }


    SyntaxValue checkSyntaxArg(int argNum, Object... args)
        throws ArgumentException
    {
        try
        {
            return (SyntaxValue) args[argNum];
        }
        catch (ClassCastException e) {}

        throw new ArgumentException(this, "Syntax value", argNum, args);
    }

    <T extends SyntaxValue> T checkSyntaxArg(Class<T> klass, String typeName,
                                             boolean nullable,
                                             int argNum, Object... args)
        throws ArgumentException
    {
        Object arg = args[argNum];

        try
        {
            SyntaxValue stx = (SyntaxValue) arg;
            if (nullable || ! stx.isAnyNull())
            {
                return klass.cast(stx);
            }
        }
        catch (ClassCastException e) {}

        throw new ArgumentException(this, typeName, argNum, args);
    }


    SyntaxSymbol checkSyntaxSymbolArg(int argNum, Object... args)
        throws ArgumentException
    {
        return checkSyntaxArg(SyntaxSymbol.class, "syntax symbol",
                              true /* nullable */, argNum, args);
    }


    SyntaxContainer checkSyntaxContainerArg(int argNum, Object... args)
        throws ArgumentException
    {
        return checkSyntaxArg(SyntaxContainer.class,
                              "syntax_list, sexp, or struct",
                              true /* nullable */, argNum, args);
    }



    SyntaxSequence checkSyntaxSequenceArg(int argNum, Object... args)
        throws ArgumentException
    {
        return checkSyntaxArg(SyntaxSequence.class,
                              "syntax_list or syntax_sexp",
                              true /* nullable */, argNum, args);
    }

    /** Ensures that an argument is a {@link Procedure}. */
    Procedure checkProcArg(int argNum, Object... args)
        throws ArgumentException
    {
        try
        {
            return (Procedure) args[argNum];
        }
        catch (ClassCastException e) {}

        throw new ArgumentException(this, "procedure", argNum, args);
    }

}
