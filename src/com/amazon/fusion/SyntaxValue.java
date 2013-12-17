// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import java.io.IOException;

/**
 * Models Fusion source code, using a custom DOM implementation of Ion.
 * Unlike the {@link IonValue} model, this one allows sharing of nodes in a
 * DAG structure.
 */
abstract class SyntaxValue
    extends BaseValue
{
    /** A zero-length array. */
    final static SyntaxValue[] EMPTY_ARRAY = new SyntaxValue[0];

    private final SourceLocation mySrcLoc;

    /**
     *
     * @param loc may be null;
     */
    SyntaxValue(SourceLocation loc)
    {
        mySrcLoc = loc;
    }


    /**
     * Determines whether the wrapped datum is a null value.
     */
    @Override
    abstract boolean isAnyNull();


    /**
     * Gets the location associated with this syntax node, if it exists.
     * @return may be null.
     */
    SourceLocation getLocation()
    {
        return mySrcLoc;
    }


    /**
     * Prepends a wrap onto our existing wraps.
     * This will return a new instance as necessary to preserve immutability.
     */
    SyntaxValue addWrap(SyntaxWrap wrap)
        throws FusionException
    {
        return this;
    }

    /**
     * Prepends a sequence of wraps onto our existing wraps.
     * This will return a new instance as necessary to preserve immutability.
     */
    SyntaxValue addWraps(SyntaxWraps wraps)
        throws FusionException
    {
        return this;
    }


    SyntaxValue addOrRemoveMark(int mark)
        throws FusionException
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
        throws FusionException
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
    abstract CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException;


    /**
     * Unwraps syntax, returning plain values. Only one layer is unwrapped, so
     * if this is a container, the result will contain syntax objects.
     */
    abstract Object unwrap(Evaluator eval)
        throws FusionException;


    /**
     * Unwraps syntax recursively, returning plain values.
     * Used by `quote` and `synatax_to_datum`.
     */
    abstract Object syntaxToDatum(Evaluator eval)
        throws FusionException;


    @Override
    SyntaxValue toStrippedSyntaxMaybe(Evaluator eval)
        throws FusionException
    {
        // TODO FUSION-183 Should strip location and properties?
        //      Well, probably not, that throws away existing
        //      context when called from datum_to_syntax
        return stripWraps(eval);
    }


    @Override
    final void write(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        IonWriter writer = WRITER_BUILDER.build(out);
        ionize(eval, writer);
        writer.flush();
    }
}
