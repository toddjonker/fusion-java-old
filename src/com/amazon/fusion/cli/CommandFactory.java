// Copyright (c) 2005-2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

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
                new Eval(),
                new Separator(),
                new Help(),
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
    public static Command makeCommand(String[] commandLine)
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
        }
        else
        {
            int argCount = commandLine.length - 1;
            String[] args = new String[argCount];
            System.arraycopy(commandLine, 1, args, 0, argCount);

            boolean commandLineOk = cmd.prepare(args);

            if (! commandLineOk)
            {
                System.err.print("Usage: ");
                System.err.println(cmd.getHelpUsage());
                System.err.print("Type '" + APP_NAME + " help ");
                System.err.print(command);
                System.err.println("' for more information.");

                return null;
            }
        }

        return cmd;
    }

    /**
     *
     */
    public static boolean executeCommandLine(String[] commandLine)
    {
        boolean success = true;

        int curStartPos = 0;
        for (int i = 0; i < commandLine.length && success; i++)
        {
            if (";".equals(commandLine[i])) {
                int len = i-curStartPos;
                if (len > 0) {
                    Command cmd = doExecutePartial(commandLine,
                                                   curStartPos,
                                                   len);
                    if (cmd == null) {
                        success = false;
                    }
                }
                curStartPos = i+1;
            }
        }
        int len = commandLine.length-curStartPos;
        if (success) {
            Command cmd = doExecutePartial(commandLine,
                                           curStartPos,
                                           len);
            success = (cmd != null);
        }
        return success;
    }

    private static Command doExecutePartial(String[] commandLine,
                                            int start,
                                            int len)
    {
        String[] partial = new String[len];
        System.arraycopy(commandLine, start, partial, 0, len);
        Command cmd = makeCommand(partial);
        if (cmd != null)
        {
            cmd.execute();
        }

        return cmd;
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
        public boolean processArguments(String[] arguments)
        {
            return false;
        }

        @Override
        public void execute()
        {
            // NO OP
        }
    }
}
