#/usr/bin/env python

import sys
import json
import bz2
import datetime
#Untested code
#start using this soon rather than just tracking plain old tweet IDs
class Node:
  def __init__(self):
    self.text = ""
    self.created = datetime.datetime.now()
    self.id = 0

  def setID(self, n):
    self.id = n
  def getID(self):
    return self.id
  @staticmethod
  def stringMonth2int(s):
    if s == "Jan": return 1
    if s == "Feb": return 2
    if s == "Mar": return 3
    if s == "Apr": return 4
    if s == "May": return 5
    if s == "Jun": return 6
    if s == "Jul": return 7
    if s == "Aug": return 8
    if s == "Sep": return 9
    if s == "Oct": return 10
    if s == "Nov": return 11
    if s == "Dec": return 12
    return 13 #this will cause a failure

  def setCreatedTime(self,s):
    #Mon Mar 11 15:30:27 +0000 2013 -> datetime object
    slist = s.split(" ")
    #year, month, day[, hour[, minute[, second[, microsecond[, tzinfo]]]]]
    timelist = slist[3].split(":")
    dateANDtime = datetime.datetime(int(slist[5]), Node.stringMonth2int(slist[1]), int(slist[2]), int(timelist[0]), int(timelist[1]), int(timelist[2]))
    self.created = dateANDtime

  def getCreatedTime(self):
    return self.created
    
  def setText(self,s): 
    self.text = s
  def getText(self):
    return self.text

class DriverIDsOnly:
  #id -> set of id  map, like a set of production rules
  #id -> [id,id...]
  #id -> [id]
  #...
  def __init__(self):
    self.id2sets = dict()
    #set of all entries;determining if an id is a root or not requires we either 
    #search all mappings to determine if we've ever seen it, or maintain some 
    #way of answering that question
    self.universe = set()
    #set of roots
    self.rootset = set()

  def update_mappings(self,id,rid):
    if rid in self.id2sets:
      #add to set (branch in flow), & add it to set of all know id's (so we know
      #it is not a root in future)
      self.id2sets[rid] = self.id2sets[rid].add(id)
      self.universe.add(id)
    else:
      #add to keys and start new mapping (continuation of a flow), and make note
      #of it being a new root & add it to set of all known id's (next 3 lines
      #fail if attempted as 1 line)
      temps = set()
      temps.add(id)
      self.id2sets[rid] = temps
      #check for pre-existence in any set and add to set of roots if it is new
      if rid not in self.universe:
        self.rootset.add(rid)
        self.universe.add(rid)
      self.universe.add(id)

  def ProcessTweet(self, tweet):
    if 'retweeted_status' in tweet.keys():
      #id is is unique id of this tweet, a retweet of a prior tweet
      id = tweet['id']
      #rid is the unique id of the tweet that is being retweeted
      rid = tweet['retweeted_status']['id']
      self.update_mappings(id, rid)


  #somewhat pretty print the dictionary--fixed bug from prior file
  def printDictionary(self, f):
    outfile = open(f,'w')
    keys = self.id2sets.keys()
    for k in keys:
      mapping = self.id2sets[k]
      outfile.write("\n")
      outfile.write(k)
      outfile.write("  ->  ")
      x = 0
      for id in mapping:
        if x > 0: print "\n\t\t | "
        outfile.write(str(altnode.getID()))
        outfile.write(id)
        x = x + 1
    outfile.flush()
    outfile.close()

  def printDictionary_toScreen(self, d):
    keys = d.keys()
    for k in keys:
      mapping = d[k]
      print("\n")
      print(str(k.getID()))
      print(" -> ")
      x = 0
      for altnode in mapping:
        if x > 0: print "\n\t| "
        print(str(k.getID()))
        x = x + 1

  def process_TweetsFromFile(self, fnout, outfile):
  # Figure out which function should open the file.
    fopen = open
    if fnout.endswith('.bz2'):
      fopen = bz2.BZ2File

    with fopen(fnout, 'rb') as fh:
      # Process every line as its own JSON tweet.
      for line in fh:
        try:
          loaded = json.loads(line)
          self.ProcessTweet(loaded)
      #print the dictionary to out file even if we interrupt further progress
        except KeyboardInterrupt:
          self.printDictionary(outfile)
          return
          raise KeyboardInterrupt
        self.printDictionary(outfile)
        
  #incase have to 2 go to file to get it, but this would be slow
  def getTweetText(self, id, f):
    for line in f:
      if 'id' in tweet.keys():
        tid = tweet['id']
        if id == tid:
          if 'text' in tweet.keys():
            return tweet['text']


class DriverNodes:
  #node -> set of node   map, like a set of production rules
  #node -> [node,node...]
  #node -> [node]
  #...
  def __init__(self):
    self.n2sets = dict()
    #set of all entries;determining if an id is a root or not requires we either 
    #search all mappings to determine if we've ever seen it, or maintain some 
    #way of answering that question
    self.universe = set()
    #set of roots
    self.rootset = set()

  #rt is retweetED tweet, t is (re)tweet, mapping is from tweet -> set of tweet
  #where retweetED tweet rt is root, (re)tweet t is in set
  def update_mappings(self, rt, t):
    if rt in self.n2sets:
      #add t to set (branch in flow), & add t to set of all know tweets (so we know
      #t is not a root in future)
      self.n2sets[rt] = self.n2sets[rt].add(t)
      self.universe.add(t)
    else:
      #add to keys and start new mapping (continuation of a flow), and make note
      #of it being a new root & add it to set of all known id's (next 3 lines
      #fail if attempted as 1 line)
      temps = set()
      temps.add(t)
      self.n2sets[rt] = temps
      #check for pre-existence in any set and add to set of roots if it is new
      if rt not in self.universe:
        self.rootset.add(rt)
        self.universe.add(rt)
      self.universe.add(t)

  def ProcessTweet(self, tweet):
    if 'retweeted_status' in tweet.keys():
      #id is unique id of this tweet, a retweet of a prior tweet
      id = tweet['id']
      tweettext = tweet['text']
      td = tweet['created_at']
      n_tweet = Node()
      n_tweet.setID(id)
      n_tweet.setText(tweettext)
      n_tweet.setCreatedTime(td)
      #-----------------
      n_retweeted = Node()
      #rid is the unique id of the tweet that is being retweeted
      rid = tweet['retweeted_status']['id']
      retweetedtext = tweet['retweeted_status']['text']
      td_retweeted = tweet['retweeted_status']['created_at']
      n_retweeted.setID(rid)
      n_retweeted.setText(retweetedtext)
      n_retweeted.setCreatedTime(td_retweeted)
      #-------------------
      self.update_mappings(n_retweeted, n_tweet)


  #somewhat pretty print the dictionary--fixed bug from prior file
  def printDictionary(self, f):
    outfile = open(f,'w')
    keys = self.n2sets.keys()
    for k in keys:
      theset = self.n2sets[k]
      outfile.write("\n")
      outfile.write(str(k.getID()))
      outfile.write("  ")
      outfile.write(str(k.getCreatedTime()))
      outfile.write("  ")
      #fails to handle unicode b/c I didn't code it up yet
#     outfile.write(str(k.getText()))
      # TODO(paul): Test if this solves the unicode problem.
      content = k.getText()
      outfile.write(content.encode('utf-8'))
      outfile.write("  ->  ")
      x = 0
      for altnode in theset:
        if x > 0: print "\n\t\t | "
        outfile.write(str(altnode.getID()))
        outfile.write("  ")
        outfile.write(str(altnode.getCreatedTime()))
        outfile.write("  ")
        #outfile.write(str(altnode.getText()))
        content = altnode.getText()
        outfile.write(content.encode('utf-8'))

        x = x + 1
    outfile.flush()
    outfile.close()

  def printMappings_toScreen(self, m):
    keys = m.keys()
    for k in keys:
      theset = m[k]
      print("\n")
      print(str(k.getID()))
      print("  ")
      print(str(k.getCreatedTime()))
      print("  ")
#      print(str(k.getText()))
      content = k.getText()
      print(content.encode('utf-8'))
      print(" -> ")
      max = len(theset)
      for x in range(max):
        if x > 0: print "\n\t| "
        altnode = theset[x]
        print(str(altnode.getID()))
        print("  ")
        print(str(altnode.getCreatedTime()))
        print("  ")
#        print(str(altnode.getText()))
        content = altnode.getText()
        print(content.encode('utf-8'))
        x = x + 1

  def process_TweetsFromFile(self, ignore, fnout, outfile):
    # Figure out which function should open the file.
    fopen = open
    if fnout.endswith('.bz2'):
      fopen = bz2.BZ2File
      
      with fopen(fnout, 'rb') as fh:
        # Process every line as its own JSON tweet.
        for line in fh:
          try:
            loaded = json.loads(line)
            self.ProcessTweet(loaded)
      #print the dictionary to out file even if we interrupt further progress
          except KeyboardInterrupt:
            self.printDictionary(outfile)
            raise KeyboardInterrupt
      self.printDictionary(outfile)

  #incase have to 2 go to file to get it, but this would be slow
  def getTweetText(id, f):
    for line in f:
      if 'id' in tweet.keys():
        tid = tweet['id']
        if id == tid:
          if 'text' in tweet.keys():
            return tweet['text']
  
def main(argv):
  driver = DriverNodes()
  #First argument is the data file to read.
  #Second argument is the data file to which to write upon completion or 
  #interruption
  driver.process_TweetsFromFile(driver, argv[1],argv[2])


if __name__=='__main__':
  main(sys.argv)
