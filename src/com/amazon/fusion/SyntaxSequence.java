// Copyright (c) 2012-2013 Amazon.com, Inc. All rights reserved.

package com.amazon.fusion;


abstract class SyntaxSequence
    extends SyntaxContainer
{
    /**
     * @param anns must not be null.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    SyntaxSequence(SourceLocation loc, String[] anns)
    {
        super(anns, loc, null);
    }

    SyntaxSequence(String[] anns, SourceLocation loc, SyntaxWraps wraps)
    {
        super(anns, loc, wraps);
    }

    abstract int size()
        throws FusionException;


    abstract SyntaxValue get(Evaluator eval, int index)
        throws FusionException;


    /**
     * Gets all the children of this sequence as a new array.
     * Useful for making changes and then building a replacement sequence.
     *
     * @return a new array.
     */
    abstract SyntaxValue[] extract(Evaluator eval)
        throws FusionException;


    /** Creates a new sequence with this + that. */
    abstract SyntaxSequence makeAppended(Evaluator eval, SyntaxSequence that)
        throws FusionException;


    /**
     * @return null if this sequence isn't proper and from goes beyond the end.
     */
    abstract SyntaxSequence makeSubseq(Evaluator eval, int from)
        throws FusionException;
}
