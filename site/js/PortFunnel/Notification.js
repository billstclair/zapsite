//////////////////////////////////////////////////////////////////////
//
// Notification.js
// JavaScript runtime code for Elm Notification module.
// Copyright (c) 2018-2021 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

(function(scope) {
  var moduleName = 'Notification';
  var sub;

  var tags =
      { isAvailable: "isAvailable",
        getPermission: "getPermission",
        requestPermission: "requestPermission",
        sendNotification: "sendNotification",
        dismissNotification:  "dismissNotification",
        lookupNotification:  "lookupNotification",
        wasAvailable:  "wasAvailable",
        gotPermission:  "gotPermission",
        notification:  "notification",
        onClick:  "onClick",
        error:  "error"
      }


  var isAvailable =
      window.Notification != null;

  var id = 0;
  var notifications = [];  

  function init() {
    var PortFunnel = scope.PortFunnel;
    if (!PortFunnel || !PortFunnel.sub || !PortFunnel.modules) {
      // Loop until PortFunnel.js has initialized itself.
      setTimeout(init, 10);
      return;
    }
    
    sub = PortFunnel.sub;
    PortFunnel.modules[moduleName] = { cmd: dispatcher };

    // Let the Elm code know we've started, with an IsAvailableAnswer message
    sub.send({ module: moduleName,
               tag: tags.wasAvailable,
               args : isAvailable
             });
  }

  init();

  function returnError(msg) {
      return { moduleName: moduleName,
               tag: tags.error,
               args: msg
             }
  }

  function ifAvailable(res) {
      if (isAvailable) {
          res.module = moduleName;
          return res;
      } else {
            returnError('Notifications are not available.')
      }
  }

  function dispatcher(tag, args) {
    if (tag == tags.isAvailable) {
      return { module: moduleName,
               tag: tags.wasAvailable,
               args: isAvailable
             };
    } else if (tag == tags.getPermission) {
        return ifAvailable({ tag: tags.gotPermission,
                             args: Notification
                           });
    } else if (tag == tags.requestPermission) {
        // This is the old callback version. More compatible.
        Notification.requestPermission(function(res) {
            sub.send({ module: moduleName,
                       tag: tags.gotPermission,
                       args: res
                     });
        });
    } else if (tag == tags.sendNotification) {
        var title = args;
        if (isAvailable) {
            notification = new Notification(title);
            newid = id++;
            notifications[newid] = notification;
            notification.onclose = function() {
                delete notifications[newid]
            };
            if (isAvailable) {
                notification.onclick = function(e) {
                    sub.send({ module: moduleName,
                               tag: tags.onClick,
                               args: newid
                             })
                };
            }
            return ifAvailable({ tag: tag.notification,
                                 args: { id: newid, title: title }
                               });
        }
        else {
            ifAvailable({});
        }
    } else if (tag == tags.dismissNotification) {
        var myid = args;
        var notification = notifications[myid];
        if (notification) {
            notification.close();
            delete notifications[myid];
        } else
            return returnError('Notification does not exist: ' + myid);
    } else if (tag == tags.lookupNotification) {
        var myid = args;
        var notification = notifications[myid];
        if (notification) {
            ifAvailable({ tag: tags.notification,
                          args: { id: id, title: notification.title }
                        });
        } else {
            return returnError('Notification does not exist: ' + myid);
        }
    }
  }

        
})(this);   // Execute the enclosing function
