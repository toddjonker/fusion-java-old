// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.unsafeStructElt;
import static com.amazon.fusion.FusionStruct.unsafeStructHasKey;
import static com.amazon.fusion.FusionStruct.unsafeStructKeys;
import static com.amazon.fusion.FusionStruct.unsafeStructMerge;
import static com.amazon.fusion.FusionStruct.unsafeStructRemoveKeys;
import java.io.IOException;
import java.util.List;

/**
 * Fusion procedure to raise a unit test failure.
 */
final class CheckFailureProc
    extends Procedure
{
    CheckFailureProc()
    {
        //    "                                                                               |
        super("Raises an exception located at the given STX syntax. The MESSAGEs are\n" +
              "displayed as part of the error.",
              "stack_param", "message", DOTDOTDOT); // FIXME doc above
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

//      String message = safeDisplayManyToString(eval, args, 1);

        DynamicParameter param = (DynamicParameter) args[0];
        List<Object> stack = param.allValues(eval);

        throw new CheckFailure(stack, "check failure");
    }


    @SuppressWarnings("serial")
    private class CheckFailure
        extends FusionException
    {
        private final List<Object> myStack;

        private CheckFailure(List<Object> stack, String message)
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

            if (unsafeStructHasKey(eval, frame, "actual"))
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
        public void displayMessage(Evaluator eval, Appendable out)
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
}
