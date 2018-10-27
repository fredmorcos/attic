import std.stdio;
import std.socket;
import std.getopt;
import std.getopt: config;
import std.exception;
import std.typecons;

int main(string[] args) {
  bool safe = false;
  bool keep = false;

  string dir;
  string host;
  string port;

  try {
    GetoptResult helpInfo = getopt
      (args,
       "s", "Do not make changes to filesystem", &safe,
       "k", "Keep running after each sync", &keep,
       "a", "Address/host to listen on", &host,
       config.required, "p", "Port to listen on", &port,
       config.required, "r", "Root directory to sync", &dir);

    if (helpInfo.helpWanted) {
      defaultGetoptPrinter("bkd: Backy sync daemon", helpInfo.options);
      return 0;
    }
  } catch (Exception e) {
    stderr.writefln("Error: Cannot parse command-line arguments: %s", e.msg);
    return 1;
  }

  try {
    writefln("Local hostname: %s", Socket.hostName);
  } catch (Exception e) {
    stderr.writefln("Error: Cannot find local hostname: %s", e.msg);
  }

  Nullable!Socket sock;

  try {
    Address[] addrs = getAddress(host, port);

    foreach (Address addr; addrs) {

    }
  } catch (Exception e) {
  }

  // {
  //   scope(failure) {
  //     stderr.writeln("Error: Could not get local address info");
  //     return 1;
  //   }

  //   Address[] addrs = getAddress(host, port);
  // }

  // foreach (Address addr; addrs) {
  // }

  // bool sockValid = false;
  // TcpSocket sock;

  // try {
  //   // Address[] addrs = getAddress(host, port);

  //   foreach (Address addr; addrs) {
  //     try {
  //       sock = new TcpSocket(addr.addressFamily);
  //       sock.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, 1);
  //       sock.bind(addr);
  //       sock.listen(0);
  //       sockValid = true;
  //       stderr.writefln("Listening on %s", addr.toAddrString);
  //       break;
  //     } catch (Exception e) {
  //       stderr.writefln("Skipping %s: %s", addr.toAddrString, e.msg);
  //       delete sock;
  //       continue;
  //     }
  //   }
  // } catch (Exception e) {
  //   stderr.writefln("Error: %s", e.msg);
  //   return 1;
  // }

  // if (!sockValid) {
  //   stderr.writeln("Error: Could not listen on any address");
  //   return 1;
  // }

  // scope(exit) {
  //   sock.close;
  //   delete sock;
  // }

  // try {
  //   Socket csock = sock.accept;
  // } catch (Exception e) {
  //   stderr.writefln("Error: %s", e.msg);
  //   return 1;
  // }

  return 0;
}
