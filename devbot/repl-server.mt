import "amp" =~ [=> makeAMPServer :DeepFrozen]
import "lib/entropy/entropy" =~ [=> makeEntropy :DeepFrozen]
import "lib/entropy/pcg" =~ [=> makePCG :DeepFrozen]
exports (main)

def main(argv, => makeTCP4ServerEndpoint, => currentRuntime) :Int as DeepFrozen:
    def [_, seed] := currentRuntime.getCrypt().makeSecureEntropy().getEntropy()
    def e := makeEntropy(makePCG(seed, 0))
    def sessions := [].asMap().diverge()
    def listener(command, args, => FAIL):
        if (command == "Eval" && args =~ [=> var session := null, => sourceText]):
            if (session == null):
                session := e.nextInt(2 ** 32)
                sessions[session] := safeScope
            def [val, newScope] := eval.evalToPair(sourceText, sessions[session])
            sessions[session] := newScope
            return ["result" => M.toQuote(val), "session" => M.toString(session)]
        else:
            FAIL(`$command[${", ".join(args.getKeys())}] not recognized`)
    def amp := makeAMPServer(makeTCP4ServerEndpoint(4960))
    amp.listen(fn a {a.setResponder(listener)})
    return 0

