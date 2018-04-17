part of dread;

/// Generated class info
class ClassInfo {
  /// Class id
  final int id;

  /// Name of class
  String get name => classData.name;

  /// Class data
  final Class classData;

  /// Constructor
  ClassInfo(this.id, this.classData);
}

/// Generate contracts command
class GenCommand extends Command {
  /// Default generated file name
  static const DefaultFileName = "generated.dart";

  /// Default library name
  static const DefaultLibraryName = "generated";

  /// String name constant
  static const StringName = "String";

  /// Int name constant
  static const IntName = "int";

  /// Packet id counter
  int _counter = 0;

  /// Return description
  @override
  String get description => "Generate contracts";

  /// Return name
  @override
  String get name => "gen";

  /// Json codec
  final codec = new JsonCodec();

  /// Generate pack method from fields for class
  String genPackMethod(List<dynamic> fields) {
    if (fields != null) {
      StringBuffer packSb = new StringBuffer();
      packSb.write('final binary = new BinaryData();');

      fields.forEach((f) {
        final String name = f["Name"];
        final String type = f["Type"];

        switch(type) {
          case StringName:
            packSb.write('binary.writeStringWithLength(${name});');
            break;
          case IntName:
            packSb.write('binary.writeVarInt(${name});');
            break;
        }
      });

      packSb.write("return binary.toData();");
      return packSb.toString();
    }

    return "";
  }

  /// Generate unpack method from fields for class
  String genUnpackMethod(List<dynamic> fields) {
    if (fields != null) {
      StringBuffer unpackSb = new StringBuffer();
      unpackSb.write('final binary = new BinaryData.fromList(data);');

      fields.forEach((f) {
        final String name = f["Name"];
        final String type = f["Type"];

        switch(type) {
          case StringName:
            unpackSb.write('${name} = binary.readStringWithLength();');
            break;
          case IntName:
            unpackSb.write('${name} = binary.readVarInt();');
            break;
        }
      });

      return unpackSb.toString();
    }

    return "";
  }

  /// Constructor
  GenCommand() {
    argParser.addOption('out', abbr: 'o', help: "Output file");
  }

  /// Build class from json data
  ClassInfo buildClass(Map<String, dynamic> item) {
    _counter += 1;
    final cls = new Class((b) {
      b.name = item["Name"];
      final extendsType = item["Extends"];
      b.extend = refer(extendsType);

      final List<dynamic> fields = item["Fields"];

      b.fields.add(new Field((fb) =>
      fb
        ..name = "RequestId"
        ..type = refer(IntName)
        ..static = true
        ..modifier = FieldModifier.constant
        ..assignment = new Code(_counter.toString())));

      b.constructors.add(new Constructor((cb) => cb.initializers.add(new Code("super(RequestId)"))));

      b.methods.add(new Method((mb) {
        mb
          ..name = "create"
          ..static = true
          ..lambda = true
          ..returns = refer("SerializableBody")
          ..body = new Code("new ${b.name}()");
      }));

      if (fields != null) {
        fields.forEach((f) {
          final String name = f["Name"];
          final String type = f["Type"];

          b.fields.add(new Field((fb) =>
          fb
            ..name = name
            ..type = refer(type)));
        });
      }

      final packMethodString = genPackMethod(fields);
      final unpackMethodString = genUnpackMethod(fields);

      b.methods
        ..add(new Method((mb) =>
        mb
          ..name = "pack"
          ..annotations.add(new CodeExpression(new Code("override")))
          ..returns = refer("List<int>")
          ..body = new Code(packMethodString)));

      b.methods
        ..add(new Method((mb) =>
        mb
          ..name = "unpack"
          ..annotations.add(new CodeExpression(new Code("override")))
          ..requiredParameters.add(new Parameter((pb) =>
          pb
            ..name = "data"
            ..type = refer("List<int>")))
          ..returns = refer("void")
          ..body = new Code(unpackMethodString)));
    });

    return new ClassInfo(_counter, cls);
  }

  /// Create build transformer
  Class buildTransformer(List<ClassInfo> clsList) {
    final cls = new Class((cb) {
      cb.name = "CustomBodyTransformer";
      cb.extend = refer("BodyTransformer");
      cb.methods.add(new Method((mb) {
        mb
          ..name = "pack"
          ..annotations.add(new CodeExpression(new Code("override")))
          ..requiredParameters.add(new Parameter((pb) {
            pb.name = "body";
            pb.type = refer("SerializableBody");
          }))
          ..returns = refer("List<int>")
          ..body = new Code('final binary = new BinaryData();'
              'if (body is BodyWithRequestId) {'
              'binary.writeUInt8(body.requestId);'
              '}'
              'binary.writeList(body.pack());'
              'return binary.toData();');
      }));

      cb.methods.add(new Method((mb) {
        mb
          ..name = "unpack"
          ..annotations.add(new CodeExpression(new Code("override")))
          ..requiredParameters.add(new Parameter((pb) {
            pb.name = "data";
            pb.type = refer("List<int>");
          }))
          ..returns = refer("SerializableBody")
          ..body = new Code('final binary = new BinaryData.fromList(data);'
              'final id = binary.readUInt8();'
              'final creator =  _creators[id];'
              'if (creator == null) throw new RebelException("Unknown body id");'
              'final body = creator();'
              'body.unpack(binary.readList());'
              'return body;');
        ;
      }));


      final creatorSb = new StringBuffer();
      creatorSb.write("{");
      for (final clInfo in clsList) {
        creatorSb.write('${clInfo.name}.RequestId : ${clInfo.name}.create,');
      }
      creatorSb.write("}");

      cb.fields.add(new Field((fb) =>
      fb
        ..name = "_creators"
        ..static = true
        ..type = refer("Map<int, BodyCreatorFunc>")
        ..assignment = new Code(creatorSb.toString())));
    });
    return cls;
  }

  /// Run command
  void run() async {
    if (argResults.rest.length < 1) throw new GeneratorException("Wrong usage");

    final name = argResults.rest[0];
    final file = new File(name);
    final content = await file.readAsString();
    final settingsData = codec.decode(content);
    var libraryName = settingsData["Library"];
    if (libraryName == null)
      libraryName = DefaultLibraryName;

    final List<dynamic> data = settingsData["Contracts"];

    final emitter = new DartEmitter();
    final classes = new List<ClassInfo>();

    for (final item in data) {
      classes.add(buildClass(item));
    }

    final transformerClass = buildTransformer(classes);
    final sb = new StringBuffer();
    sb.write("part of ${libraryName};");
    sb.write("typedef SerializableBody BodyCreatorFunc();");
    sb.write("abstract class BodyWithRequestId extends SerializableBody {"
        "final int requestId;"
        "BodyWithRequestId(this.requestId);"
        "}"
        "abstract class RequestBody extends BodyWithRequestId {"
        "RequestBody(int requestId) : super(requestId);"
        "}");

    sb.write(transformerClass.accept(emitter));

    for (final cls in classes) {
      sb.write(cls.classData.accept(emitter));
    }

    final text = new DartFormatter().format(sb.toString());
    var outFile = argResults["out"];
    if (outFile == null)
      outFile = DefaultFileName;

    final fl = new File(outFile);
    await fl.writeAsString(text);
  }
}
