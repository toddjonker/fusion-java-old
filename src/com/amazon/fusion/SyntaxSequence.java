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
import java.util.Collections;
import java.util.List;

abstract class SyntaxSequence
    extends SyntaxContainer
{
    List<SyntaxValue> myChildren;


    final void readChildren(IonReader source)
    {
        if (! source.isNullValue())
        {
            ensureNotNull();
            source.stepIn();
            while (source.next() != null)
            {
                SyntaxValue child = Syntax.read(source);
                add(child);
            }
            source.stepOut();
        }
    }


    @Override
    final boolean isNullValue()
    {
        return myChildren == null;
    }

    /** Turns null into empty sequence, otherwise leaves things alone. */
    final void ensureNotNull()
    {
        if (myChildren == null)
        {
            myChildren = new ArrayList<SyntaxValue>();
        }
    }

    final int size()
    {
        return (myChildren == null ? 0 : myChildren.size());
    }

    final SyntaxValue get(int index)
    {
        return myChildren.get(index);
    }

    final void add(SyntaxValue... children)
    {
        ensureNotNull();
        Collections.addAll(myChildren, children);
    }

    final FusionValue addQuotedChildren(Evaluator eval,
                                        ValueFactory factory,
                                        IonSequence list)
    {
        assert list.isNullValue();
        int size = size();

        if (size == 0)
        {
            list.clear();
        }
        else
        {
            for (int i = 0; i < size; i++)
            {
                SyntaxValue s = get(i);
                FusionValue fv = s.quote(eval);
                IonValue iv = FusionValue.toIonValue(fv, factory);
                list.add(iv);
            }
        }

        return new DomValue(list);
    }

    final void writeTo(IonWriter writer, IonType type)
        throws IOException
    {
        if (isNullValue())
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
