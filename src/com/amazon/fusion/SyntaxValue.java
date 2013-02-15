// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.IOException;

/**
 * Models Fusion source code, using a custom DOM implementation of Ion.
 * Unlike the {@link IonValue} model, this one allows sharing of nodes in a
 * DAG structure.
 */
abstract class SyntaxValue
    extends FusionValue
{
    enum Type {
        NULL, BOOL, INT, DECIMAL, FLOAT, TIMESTAMP, BLOB, CLOB,
        STRING, SYMBOL, LIST, SEXP, STRUCT
    }

    /** A zero-lengeth array. */
    final static SyntaxValue[] EMPTY_ARRAY = new SyntaxValue[0];

    private final String[] myAnnotations;
    private final SourceLocation mySrcLoc;

    /**
     *
     * @param annotations the new instance assumes ownership of the array and
     * it must not be modified later. Must not be null.
     *
     * @param loc may be null;
     */
    SyntaxValue(String[] annotations, SourceLocation loc)
    {
        assert annotations != null : "annotations must not be null";
        myAnnotations = annotations;
        mySrcLoc = loc;
    }


    final String[] getAnnotations()
    {
        return myAnnotations;
    }

    /**
     * Gets the location associated with this syntax node, if it exists.
     * @return may be null.
     */
    SourceLocation getLocation()
    {
        return mySrcLoc;
    }

    abstract Type getType();


    /**
     * Prepends a wrap onto our existing wraps.
     * This will return a new instance as necessary to preserve immutability.
     */
    SyntaxValue addWrap(SyntaxWrap wrap)
    {
        return this;
    }

    /**
     * Prepends a sequence of wraps onto our existing wraps.
     * This will return a new instance as necessary to preserve immutability.
     */
    SyntaxValue addWraps(SyntaxWraps wraps)
    {
        return this;
    }


    SyntaxValue addOrRemoveMark(int mark)
    {
        SyntaxWrap wrap = new MarkWrap(mark);
        return addWrap(wrap);
    }


    /**
     * Removes any wraps from this value and any children.
     * @return an equivalent syntax value with no wraps.
     * May return this instance when that's already the case.
     */
    SyntaxValue stripWraps(Evaluator eval)
    {
        return this;
    }


    /** Don't call directly! Go through the evaluator. */
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        return this;
    }


    /** Don't call directly! Go through the evaluator. */
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        Object constant = doCompileIonConstant(eval, env);
        IonValue iv = FusionValue.castToIonValueMaybe(constant);
        assert iv != null;
        return new CompiledIonConstant(iv);
    }


    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        throw new IllegalStateException();
    }


    /**
     * Unwraps syntax, returning plain values.
     * @param recurse if true, unwrapping is recursive (as per `quote` or
     *  `synatax_to_datum`); otherwise only one layer is unwrapped.
     */
    abstract Object unwrap(Evaluator eval, boolean recurse)
        throws FusionException;


    @Override
    void write(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        IonWriter writer = IonTextWriterBuilder.standard().build(out);
        ionize(eval, writer);
        writer.flush();
    }

    void ionizeAnnotations(IonWriter writer)
    {
        writer.setTypeAnnotations(myAnnotations);
    }

    abstract boolean isNullValue();


    //========================================================================


    static final class CompiledIonConstant
        implements CompiledForm
    {
        private final IonValue myConstant;

        CompiledIonConstant(IonValue constant)
        {
            constant.makeReadOnly();
            myConstant = constant;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return eval.inject(myConstant.clone());
        }
    }
}
