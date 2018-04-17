part of dread;

/// Process arguments from console
class ArgumentProcessor {
  /// Console arguments
  final List<String> arguments;

  /// Constructor
  ArgumentProcessor(this.arguments);

  /// Process arguments
  void process() async {
    final commandRunner = new CommandRunner("congen", "Generates contracts for rebel server/client and body transformer");
    try {
      commandRunner.addCommand(new GenCommand());
      await commandRunner.run(arguments);
    } on GeneratorException catch (e) {
      print(commandRunner.usage);
    }
  }
}
