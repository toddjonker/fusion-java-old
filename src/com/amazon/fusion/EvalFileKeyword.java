// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonString;

/**
 * This is syntax because we currently evaluate the file in the invoker's
 * environment. That's not right but there's no access on anything else now.
 */
final class EvalFileKeyword
    extends KeywordValue
{
    private final LoadHandler myLoadHandler;

    EvalFileKeyword(LoadHandler loadHandler)
    {
        //    "                                                                               |
        super("FILENAME",
              "Opens the Fusion source file named by the given string and evaluates each\n" +
              "expression in sequence. Returns the last result.\n" +
              "FILENAME is resolve relative to the value of current_directory.");

        myLoadHandler = loadHandler;
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        check(source).arityExact(2);
        return super.expand(eval, env, source);
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxValue argSource = source.get(1);
        CompiledForm form = eval.compile(env, argSource);
        return new CompiledEvalFile(form);
    }


    //========================================================================


    private final class CompiledEvalFile
        implements CompiledForm
    {
        private final CompiledForm myFileNameForm;

        CompiledEvalFile(CompiledForm fileNameForm)
        {
            myFileNameForm = fileNameForm;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            String fileName;
            {
                Object argValue = eval.eval(store, myFileNameForm);
                try
                {
                    IonString nameDom = (IonString)
                        castToIonValueMaybe(argValue);
                    fileName = nameDom.stringValue();
                }
                catch (ClassCastException e)
                {
                    throw new ArgTypeFailure(EvalFileKeyword.this, "string",
                                             0, argValue);
                }
                catch (NullPointerException e)
                {
                    throw new ArgTypeFailure(EvalFileKeyword.this, "string",
                                             0, argValue);
                }
            }

            // TODO FUSION-55 incorrect namespace handling
            // If our lexical Store is a module, the cast will fail since
            // Namespace is a prepare-time object.
            // TODO FUSION-49 separate env and store
            Namespace namespace = (Namespace) store.namespace();
            return myLoadHandler.loadTopLevel(eval, namespace, fileName);
        }
    }
}
