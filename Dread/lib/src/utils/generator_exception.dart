part of dread;

/// Generator exception
class GeneratorException implements Exception {
  /// Exception message
  String message;

  /// Constructor
  GeneratorException(this.message);
}