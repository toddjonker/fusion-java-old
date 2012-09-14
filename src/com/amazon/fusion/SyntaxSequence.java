// Copyright (c) 2012 Amazon.com, Inc. All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static java.lang.System.arraycopy;
import static java.util.Arrays.copyOfRange;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.List;

abstract class SyntaxSequence
    extends SyntaxContainer
{
    /**
     * Both the array and its content may be shared with other instances.
     * When we push down wraps, we copy the array and the children.
     * We push lazily to aggregate as many wraps here and only push once.
     * That avoids repeated cloning of the children.
     */
    private SyntaxValue[] myChildren;


    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children the children of the new list.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param anns must not be null.
     */
    SyntaxSequence(SyntaxValue[] children, String[] anns, SourceLocation loc)
    {
        super(anns, loc, null);
        myChildren = children;
    }

    /**
     * Copy constructor, shares the myChildren array and replaces wraps.
     * The array will be copied when wraps are pushed but not before.
     */
    SyntaxSequence(SyntaxSequence that, SyntaxWraps wraps)
    {
        super(that.getAnnotations(), that.getLocation(), wraps);
        assert that.myChildren.length != 0 && wraps != null;
        myChildren = that.myChildren;
    }


    /**
     * If we have wraps cached here, push them down into fresh copies of all
     * children. This must be called before exposing any children outside of
     * this instance, so that it appears as if the wraps were pushed when they
     * were created.
     */
    private void pushAnyWraps()
    {
        if (myWraps != null)  // We only have wraps when we have children.
        {
            boolean changed = false;
            int len = myChildren.length;
            SyntaxValue[] newChildren = new SyntaxValue[len];
            for (int i = 0; i < len; i++)
            {
                SyntaxValue child = myChildren[i];
                SyntaxValue wrapped = child.addWraps(myWraps);
                newChildren[i] = wrapped;
                changed |= wrapped != child;
            }

            if (changed) myChildren = newChildren; // Keep sharing when we can

            myWraps = null;
        }
    }


    @Override
    SyntaxSequence stripWraps()
    {
        if (hasNoChildren()) return this;  // No children, no marks, all okay!

        // Even if we have no marks, some children may have them.
        boolean mustCopy = (myWraps != null);

        int len = myChildren.length;
        SyntaxValue[] newChildren = new SyntaxValue[len];
        for (int i = 0; i < len; i++)
        {
            SyntaxValue child = myChildren[i];
            SyntaxValue stripped = child.stripWraps();
            newChildren[i] = stripped;
            mustCopy |= stripped != child;
        }

        if (! mustCopy) return this;

        return makeSimilar(newChildren, getAnnotations(), getLocation());
    }


    @Override
    final boolean isNullValue()
    {
        return myChildren == null;
    }


    @Override
    final boolean hasNoChildren()
    {
        return myChildren == null || myChildren.length == 0;
    }


    final int size()
    {
        return (myChildren == null ? 0 : myChildren.length);
    }


    /**
     * Gets all the children of this sequence as a new array.
     * Useful for making changes and then building a replacement sequence.
     *
     * @return a new array.
     */
    SyntaxValue[] extract()
    {
        if (myChildren == null) return null;

        pushAnyWraps();

        int len = myChildren.length;
        SyntaxValue[] extracted = new SyntaxValue[len];
        arraycopy(myChildren, 0, extracted, 0, len);
        return extracted;
    }


    void extract(List<SyntaxValue> list, int from)
    {
        if (myChildren != null)
        {
            pushAnyWraps();
            for (int i = from; i < myChildren.length; i++)
            {
                list.add(myChildren[i]);
            }
        }
    }


    final SyntaxValue get(int index)
    {
        pushAnyWraps();
        return myChildren[index];
    }


    /**
     * Construct a null value of the appropriate type.
     * @param factory must not be null.
     * @return a new instance.
     */
    abstract IonSequence makeNull(ValueFactory factory);


    /**
     * Creates a new sequence of the same type, with the children.
     *
     * @param anns must not be null.
     */
    abstract SyntaxSequence makeSimilar(SyntaxValue[] children,
                                        String[] anns,
                                        SourceLocation loc);


    /** Creates a new sequence with this + that. */
    SyntaxSequence makeAppended(SyntaxSequence that)
    {
        int thisLength = this.size();
        int thatLength = that.size();
        int newLength  = thisLength + thatLength;

        SyntaxValue[] children;
        if (newLength == 0)
        {
            children = SyntaxValue.EMPTY_ARRAY;
        }
        else
        {
            children = new SyntaxValue[thisLength + thatLength];
            if (thisLength != 0)
            {
                this.pushAnyWraps();
                arraycopy(this.myChildren, 0, children, 0, thisLength);
            }
            if (thatLength != 0)
            {
                that.pushAnyWraps();
                arraycopy(that.myChildren, 0, children, thisLength, thatLength);
            }
        }

        return makeSimilar(children, EMPTY_STRING_ARRAY, null);
    }


    SyntaxSequence makeSubseq(int from, int to)
    {
        pushAnyWraps();
        SyntaxValue[] children =
            (myChildren == null ? null : copyOfRange(myChildren, from, to));
        return makeSimilar(children, EMPTY_STRING_ARRAY, null);
    }


    @Override
    final FusionValue quote(Evaluator eval)
    {
        ValueFactory factory = eval.getSystem();
        IonSequence seq = makeNull(factory);
        seq.setTypeAnnotations(getAnnotations());

        if (! isNullValue())
        {
            int size = size();
            if (size == 0)
            {
                seq.clear();  // Turns null sequence into empty sequence
            }
            else
            {
                for (int i = 0; i < size; i++)
                {
                    SyntaxValue s = get(i);
                    FusionValue fv = s.quote(eval);
                    IonValue iv = FusionValue.toIonValue(fv, factory);
                    seq.add(iv);
                }
            }
        }
        return new DomValue(seq);
    }

    final void writeContentTo(IonWriter writer, IonType type)
        throws IOException
    {
        if (myChildren == null)
        {
            writer.writeNull(type);
        }
        else
        {
            writer.stepIn(type);
            for (SyntaxValue child : myChildren)
            {
                child.writeTo(writer);
            }
            writer.stepOut();
        }
    }
}
