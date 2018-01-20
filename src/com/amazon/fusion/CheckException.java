// Copyright (c) 2014-2018 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.unsafeStructElt;
import static com.amazon.fusion.FusionStruct.unsafeStructHasKey;
import static com.amazon.fusion.FusionStruct.unsafeStructKeys;
import static com.amazon.fusion.FusionStruct.unsafeStructMerge;
import static com.amazon.fusion.FusionStruct.unsafeStructRemoveKeys;
import java.io.IOException;
import java.util.List;

@SuppressWarnings("serial")
final class CheckException
    extends FusionErrorException
{
    private final List<Object> myStack;

    CheckException(List<Object> stack, String message)
    {
        super(message);
        myStack = stack;
    }

    private void displayField(Evaluator eval, Appendable out,
                              String fieldName, Object value)
        throws IOException, FusionException
    {
        out.append(fieldName);
        out.append(": ");
        FusionIo.write(eval, out, value);
        out.append('\n');
    }

    private void displayField(Evaluator eval, Appendable out,
                              Object frame, String fieldName)
        throws IOException, FusionException
    {
        if (unsafeStructHasKey(eval, frame, fieldName))
        {
            Object v = unsafeStructElt(eval, frame, fieldName);
            displayField(eval, out, fieldName, v);
        }
    }

    private void displayFrame(final Evaluator  eval,
                              final Appendable out,
                              Object frame)
        throws IOException, FusionException
    {
        displayField(eval, out, frame, "name");
        displayField(eval, out, frame, "expression");

        if (unsafeStructHasKey(eval, frame, "actual") ||
            unsafeStructHasKey(eval, frame, "expected"))
        {
            displayField(eval, out, frame, "actual");
            displayField(eval, out, frame, "expected");
        }
        else
        {
            displayField(eval, out, frame, "args");
        }

        frame = unsafeStructRemoveKeys(eval, frame,
                                       new String[]{ "name",
                                                     "expression",
                                                     "actual",
                                                     "expected",
                                                     "args" });

        for (String fieldName : unsafeStructKeys(eval, frame))
        {
            Object value = unsafeStructElt(eval, frame, fieldName);
            displayField(eval, out, fieldName, value);
        }
    }


    @Override
    void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        out.append("Check failure:");

        Object frame = null;
        for (Object entry : myStack)
        {
            if (frame == null)
            {
                frame = entry;
            }
            else
            {
                frame = unsafeStructMerge(eval, frame, entry);
            }

            if (unsafeStructHasKey(eval, frame, "name"))
            {
                out.append('\n');
                displayFrame(eval, out, frame);
                frame = null;
            }
        }

        if (frame != null)
        {
            out.append('\n');
            displayFrame(eval, out, frame);
        }
    }
}
