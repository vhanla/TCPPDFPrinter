object Service1: TService1
  DisplayName = 'PDF Printer Monitoring Service'
  AfterInstall = ServiceAfterInstall
  OnExecute = ServiceExecute
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 480
  Width = 640
end
