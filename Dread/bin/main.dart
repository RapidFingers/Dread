import 'package:dread/dread_lib.dart';

/// Entry point
main(List<String> arguments) async {
  final processor = new ArgumentProcessor(arguments);
  await processor.process();
}