// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 */
public class InterruptionTest
    extends CoreTestCase
{
    private abstract static class EvaluationTask
        implements Runnable
    {
        volatile Throwable   myCaughtThrowable;
        volatile boolean     myEvaluationFinished;

        abstract void eval() throws Exception;

        @Override
        public final void run()
        {
            try
            {
                eval();
                myEvaluationFinished = true;
            }
            catch (Throwable t)
            {
                myCaughtThrowable = t;
            }
        }
    }


    @Test
    public void threadInterruptStopsEvaluation()
        throws Exception
    {
        // Bootstrap our FusionRuntime with the bare minimum.
        runtimeBuilder().setDefaultLanguage("/fusion/private/kernel");
        final FusionRuntime runtime = runtime();

        EvaluationTask task = new EvaluationTask()
        {
            @Override
            void eval() throws Exception
            {
                runtime.makeTopLevel("/fusion");
            }
        };

        final Thread thread = new Thread(task);
        thread.start();

        Thread.sleep(10);
        assertNull(task.myCaughtThrowable);
        assertFalse("Slept for too long, all of /fusion was loaded",
                    task.myEvaluationFinished);

        thread.interrupt();
        thread.join();
        assertTrue(task.myCaughtThrowable instanceof FusionInterruptedException);
        assertFalse(task.myEvaluationFinished);

        // The code above aborts compiling and loading of /fusion.
        // Now check that we can can successfully load the rest of /fusion.

        runtime.makeTopLevel("/fusion");
    }
}
