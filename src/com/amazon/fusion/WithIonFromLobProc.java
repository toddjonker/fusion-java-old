// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionLob.isLob;
import static com.amazon.fusion.FusionLob.unsafeLobBytesNoCopy;
import com.amazon.ion.IonReader;
import java.io.IOException;

/**
 *
 */
final class WithIonFromLobProc
    extends Procedure
{
    private final DynamicParameter myCurrentIonReaderParam;


    public WithIonFromLobProc(Object currentIonReaderParam)
    {
        //    "                                                                               |
        super("Uses the content of `lob` (a blob or clob) as the current Ion input stream\n" +
              "while applying the `thunk`.\n" +
              "\n" +
              "The `lob` must be non-null and may contain Ion text or binary data.\n" +
              "The `thunk` must be a procedure that accepts zero arguments.\n" +
              "\n" +
              "A common use of this procedure is to read a single Ion value from the data:\n" +
              "\n" +
              "    (with_ion_from_lob {{\"[only_me]\"}} read)   --> [only_me]\n" +
              "\n" +
              "This leverages the fact that `read` accepts zero arguments and consumes the\n" +
              "current Ion input stream.",
              "lob", "thunk");

        myCurrentIonReaderParam = (DynamicParameter) currentIonReaderParam;
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        Object lob = args[0];
        if (! isLob(eval, lob) || isAnyNull(eval, lob).isTrue())
        {
            throw argFailure("non-null blob or clob", 0, args);
        }

        Procedure thunk = checkProcArg(1, args);
        // TODO FUSION-85 check thunk arity

        byte[] bytes = unsafeLobBytesNoCopy(eval, lob);

        try (IonReader reader = eval.getSystem().newReader(bytes))
        {
            Evaluator parameterized =
                eval.markedContinuation(myCurrentIonReaderParam, reader);

            // We cannot use a tail call here, since we must not close the
            // stream until after the call returns.
            return parameterized.callNonTail(thunk);
        }
        catch (IOException e)
        {
            // TODO improve error message
            throw new FusionException(e);
        }
    }
}
