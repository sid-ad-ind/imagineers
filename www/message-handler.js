Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    //alert(JSON.stringify(message));
     $('.result').html('<br/>'+message);
  }
);