// Copyright (c) 2012 Amazon.com, Inc. All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

abstract class SyntaxSequence
    extends SyntaxContainer
{
    private final List<SyntaxValue> myChildren;

    SyntaxSequence(String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myChildren = null;
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children this instance takes ownership and it must not be modified!
     */
    SyntaxSequence(String[] anns, SourceLocation loc,
                   List<SyntaxValue> children)
    {
        super(anns, loc);
        myChildren = children;
    }


    static List<SyntaxValue> readChildren(IonReader source)
    {
        if (source.isNullValue()) return null;

        List<SyntaxValue> children = new ArrayList<SyntaxValue>();

        source.stepIn();
        while (source.next() != null)
        {
            SyntaxValue child = Syntax.read(source);
            children.add(child);
        }
        source.stepOut();

        return children;
    }


    void pushAnyWraps()
    {
        if (myWraps != null)
        {
            if (myChildren != null)
            {
                for (SyntaxValue child : myChildren)
                {
                    child.addWraps(myWraps);
                }
            }
            myWraps = null;
        }
    }

    @Override
    final boolean isNullValue()
    {
        return myChildren == null;
    }


    final int size()
    {
        return (myChildren == null ? 0 : myChildren.size());
    }

    final SyntaxValue get(int index)
    {
        pushAnyWraps();
        return myChildren.get(index);
    }


    /** Creates a new sequence of the same type, with the children. */
    abstract SyntaxSequence makeSimilar(List<SyntaxValue> children);


    /** Creates a new sequence with this + the other. */
    SyntaxSequence makeAppended(SyntaxSequence that)
    {
        ArrayList<SyntaxValue> children =
            new ArrayList<SyntaxValue>(this.size() + that.size());
        if (this.myChildren != null)
        {
            this.pushAnyWraps();
            children.addAll(this.myChildren);
        }
        if (that.myChildren != null)
        {
            that.pushAnyWraps();
            children.addAll(that.myChildren);
        }

        return makeSimilar(children);
    }

    SyntaxSequence makeSubseq(int from, int to)
    {
        pushAnyWraps();
        List<SyntaxValue> children =
            (myChildren == null ? null : myChildren.subList(from, to));
        return makeSimilar(children);
    }


    /**
     * Construct a null value of the appropriate type.
     * @param factory must not be null.
     * @return a new instance.
     */
    abstract IonSequence makeNull(ValueFactory factory);


    /*final*/ @Override
    FusionValue quote(Evaluator eval)
    {
        ValueFactory factory = eval.getSystem();
        IonSequence seq = makeNull(factory);
        seq.setTypeAnnotations(getAnnotations());

        int size = size();

        if (size == 0)
        {
            seq.clear();
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
