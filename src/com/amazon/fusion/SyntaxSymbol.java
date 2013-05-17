// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.Set;

final class SyntaxSymbol
    extends SyntaxText
{
    /** A zero-length array of {@link SyntaxSymbol}. */
    static final SyntaxSymbol[] EMPTY_ARRAY = new SyntaxSymbol[0];

    /** Extract the names from an array of symbols. */
    static String[] toNames(SyntaxSymbol[] symbols)
    {
        if (symbols == null || symbols.length == 0)
        {
            return FusionUtils.EMPTY_STRING_ARRAY;
        }
        else
        {
            String[] names = new String[symbols.length];
            for (int i = 0; i < symbols.length; i++)
            {
                names[i] = symbols[i].stringValue();
            }
            return names;
        }
    }


    /** Initialized during {@link #prepare} */
    private Binding myBinding;

    final SyntaxWraps myWraps;

    /**
     * @param anns the new instance assumes ownership of the array and
     * it must not be modified later. Must not be null.
     */
    private SyntaxSymbol(String name, String[] anns, SourceLocation loc,
                         SyntaxWraps wraps)
    {
        super(name, anns, loc);
        myWraps = wraps;
    }

    static SyntaxSymbol make(String name)
    {
        return new SyntaxSymbol(name, EMPTY_STRING_ARRAY, null, null);
    }

    static SyntaxSymbol make(String name, SyntaxWrap wrap)
    {
        return new SyntaxSymbol(name, EMPTY_STRING_ARRAY, null,
                                SyntaxWraps.make(wrap));
    }

    static SyntaxSymbol make(String name, String[] anns, SourceLocation loc)
    {
        return new SyntaxSymbol(name, anns, loc, null);
    }

    /**
     * @param wraps may be null.
     */
    private SyntaxSymbol copyReplacingWraps(SyntaxWraps wraps)
    {
        // We intentionally don't copy the binding, since the wraps are
        // probably different, so the binding may be different.

        SyntaxSymbol copy =
            new SyntaxSymbol(myText, getAnnotations(), getLocation(), wraps);
        return copy;
    }

    SyntaxSymbol copyReplacingBinding(Binding binding)
    {
        SyntaxSymbol copy =
            new SyntaxSymbol(myText, getAnnotations(), getLocation(), myWraps);
        copy.myBinding = binding;
        return copy;
    }


    @Override
    Type getType()
    {
        return Type.SYMBOL;
    }


    @Override
    SyntaxSymbol addWrap(SyntaxWrap wrap)
    {
        SyntaxWraps newWraps;
        if (myWraps == null)
        {
            newWraps = SyntaxWraps.make(wrap);
        }
        else
        {
            newWraps = myWraps.addWrap(wrap);
        }
        return copyReplacingWraps(newWraps);
    }

    @Override
    SyntaxSymbol addWraps(SyntaxWraps wraps)
    {
        SyntaxWraps newWraps;
        if (myWraps == null)
        {
            newWraps = wraps;
        }
        else
        {
            newWraps = myWraps.addWraps(wraps);
        }
        return copyReplacingWraps(newWraps);
    }


    @Override
    SyntaxSymbol stripWraps(Evaluator eval)
    {
        if (myWraps == null) return this;
        return copyReplacingWraps(null);
    }

    /**
     * Adds the wraps on this symbol onto those already on another value.
     * @return syntax matching the source, after adding the wraps from this
     * symbol.
     */
    SyntaxValue copyWrapsTo(SyntaxValue source)
    {
        if (myWraps == null) return source;
        return source.addWraps(myWraps);
    }

    /**
     * @return not null.
     */
    Set<Integer> computeMarks()
    {
        if (myWraps == null) return Collections.emptySet();
        return myWraps.computeMarks();
    }


    /** Not set until {@link #resolve} or {@link #doExpand}. */
    Binding getBinding()
    {
        return myBinding;
    }

    /**
     * Expand-time binding resolution.
     * As a postcondition, {@link #myBinding} is not null.
     *
     * @return not null.
     */
    Binding resolve()
    {
        if (myBinding == null)
        {
            String name = stringValue();
            // TODO FUSION-114 check that our text has at least one character!
            if (myWraps == null)
            {
                myBinding = new FreeBinding(name);
            }
            else
            {
                myBinding = myWraps.resolve(name);
                if (myBinding == null) myBinding = new FreeBinding(name);
            }
        }
        return myBinding;
    }


    /**
     * Resolves this identifier, but doesn't cache the result if it has not
     * been previously resolved.
     *
     * @return not null, but maybe a {@link FreeBinding}.
     */
    Binding uncachedResolve()
    {
        if (myBinding != null) return myBinding;
        String name = stringValue();
        if (myWraps != null)
        {
            Binding b = myWraps.resolve(name);
            if (b != null) return b;
        }
        return new FreeBinding(name);
    }


    /**
     * Resolves this identifier, but doesn't cache the result if it has not
     * been previously resolved.
     *
     * @return null is equivalent to a {@link FreeBinding}.
     */
    Binding uncachedResolveMaybe()
    {
        if (myBinding != null) return myBinding;
        if (myWraps   == null) return null;
        String name = stringValue();
        return myWraps.resolve(name);
    }


    /**
     * Copies this identifier, caching a top-resolved binding.
     * @return not null.
     */
    SyntaxSymbol copyAndResolveTop()
    {
        Binding b = null;
        if (myWraps != null)
        {
            String name = stringValue();
            b = myWraps.resolveTop(name);
        }
        if (b == null)
        {
            b = new FreeBinding(stringValue());
        }

        return copyReplacingBinding(b);
    }


    /**
     * Determines whether this identifier resolves to a {@link FreeBinding}
     * with the given name and marks.
     */
    boolean resolvesFree(String name, Set<Integer> marks)
    {
        Binding resolvedBoundId = resolve();
        if (resolvedBoundId.isFree(name))
        {
            Set<Integer> boundMarks = computeMarks();
            if (marks.equals(boundMarks))
            {
                return true;
            }
        }
        return false;
    }


    /**
     * Determines whether this identifier resolves to a given binding and has
     * the given marks.
     */
    boolean resolvesBound(Binding binding, Set<Integer> marks)
    {
        Binding resolvedBoundId = resolve();
        if (resolvedBoundId.sameTarget(binding))
        {
            Set<Integer> boundMarks = computeMarks();
            if (marks.equals(boundMarks))
            {
                return true;
            }
        }
        return false;
    }


    /**
     * Checks if this symbol is bound to a {@link SyntacticForm} in the given
     * enviroment.  If so, cache the binding and return the form.  Otherwise
     * do nothing.
     *
     * @return may be null.
     */
    SyntacticForm resolveSyntaxMaybe(Environment env)
    {
        // TODO FUSION-114 what if this is null or empty symbol?
        //      We shouldn't fail in that case, at least not yet.

        Binding b = null;
        if (myBinding != null)
        {
            b = myBinding;
        }
        else if (myWraps != null)
        {
            b = myWraps.resolve(stringValue());
        }

        if (b != null)
        {
            Object resolved = b.lookup(env);
            if (resolved instanceof SyntacticForm)
            {
                myBinding = b;
                return (SyntacticForm) resolved;
            }
        }
        return null;
    }


    @Override
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        if (myBinding == null)        // Otherwise we've already been expanded
        {
            if (myText == null)
            {
                throw new SyntaxFailure(null,
                                        "null.symbol is not an expression",
                                        this);
            }

            resolve();

            if (myBinding instanceof FreeBinding)
            {
                SyntaxSymbol top =
                    new SyntaxSymbol("#%top", EMPTY_STRING_ARRAY,
                                     null, myWraps);
                if (top.resolve() instanceof FreeBinding)
                {
                    throw new UnboundIdentifierFailure(myText, this);
                }

                assert getAnnotations().length == 0;
                SyntaxSexp topExpr = SyntaxSexp.make(expander, top, this);

                // TODO TAIL
                return expander.expandExpression(env, topExpr);
            }
        }

        return this;
    }


    boolean freeIdentifierEqual(SyntaxSymbol that)
    {
        Binding thisBinding = this.uncachedResolve();
        Binding thatBinding = that.uncachedResolve();
        return thisBinding.sameTarget(thatBinding);
    }

    Object freeIdentifierEqual(Evaluator eval, SyntaxSymbol that)
    {
        return eval.newBool(freeIdentifierEqual(that));
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newSymbol(myText, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeSymbol(myText);
    }

    String debugString()
    {
        String base = toString();
        Set<Integer> marks = this.computeMarks();
        if (marks.isEmpty()) return base;
        for (Integer mark : marks)
        {
            base += "#" + mark;
        }
        return base;
    }

    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        assert myBinding != null : "No binding for " + myText;

        if (myBinding instanceof FreeBinding)
        {
            throw new UnboundIdentifierFailure(myText, this);
        }

        return myBinding.compileReference(eval, env);
    }
}
