// Copyright (c) 2005-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.cli.Command.Executor;

class CommandFactory
{
    public static final String APP_NAME = "fusion";

    public static final String TYPE_HELP_MSG =
        "Type '" + APP_NAME + " help' for usage.";

    public static Command[] getAllCommands()
    {
        Command[] commands =
            new Command[]
            {
                new Repl(),
                new Load(),
                new Cover(),
                new Separator(),
                new Help(),
                new Version(),
                new Document(),
            };

        return commands;
    }


    public static Command getMatchingCommand(String subcommand)
    {
        Command[] allCommands = getAllCommands();

        for (Command cmd : allCommands)
        {
            if (cmd.matches(subcommand))
            {
                return cmd;
            }
        }

        return null;
    }


    /**
     * @param commandLine must not be <code>null</code>.
     *
     * @return <code>null</code> if the command-line is unusable.
     */
    public static Executor makeCommand(String[] commandLine)
    {
        if (commandLine.length == 0)
        {
            System.err.println(TYPE_HELP_MSG);
            return null;
        }

        String command = commandLine[0];

        Command cmd = getMatchingCommand(command);
        if (cmd == null)
        {
            System.err.println("Unknown command: '" + command + "'");
            System.err.println(TYPE_HELP_MSG);
            return null;
        }

        int argCount = commandLine.length - 1;
        String[] args = new String[argCount];
        System.arraycopy(commandLine, 1, args, 0, argCount);

        Executor exec = cmd.prepare(args);
        if (exec == null)
        {
            System.err.print("Usage: ");
            System.err.println(cmd.getHelpUsage());
            System.err.print("Type '" + APP_NAME + " help ");
            System.err.print(command);
            System.err.println("' for more information.");

            return null;
        }

        return exec;
    }


    /**
     * @return an error code, zero meaning success.
     */
    public static int executeCommandLine(String[] commandLine)
        throws Exception
    {
        int errorCode = 0;

        int curStartPos = 0;
        for (int i = 0; i < commandLine.length && errorCode == 0; i++)
        {
            if (";".equals(commandLine[i])) {
                int len = i-curStartPos;
                if (len > 0) {
                    errorCode = doExecutePartial(commandLine,
                                                 curStartPos,
                                                 len);
                }
                curStartPos = i+1;
            }
        }
        int len = commandLine.length-curStartPos;
        if (errorCode == 0) {
            errorCode = doExecutePartial(commandLine, curStartPos, len);
        }

        return errorCode;
    }


    /**
     * @return an error code, zero meaning success.
     */
    private static int doExecutePartial(String[] commandLine,
                                        int start,
                                        int len)
        throws Exception
    {
        String[] partial = new String[len];
        System.arraycopy(commandLine, start, partial, 0, len);
        Executor cmd = makeCommand(partial);
        if (cmd != null)
        {
            return cmd.execute();
        }

        return 1;
    }

    static class Separator extends Command
    {

        Separator() {
            super("-----");
            putHelpText("-------------------------", "-----", "-----");
        }

        @Override
        boolean matches(String command)
        {
            return false;
        }

        @Override
        public Executor processArguments(String[] arguments)
        {
            return null;
        }
    }
}
