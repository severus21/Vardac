import asyncio
import logging
from abc import ABC, abstractmethod
import os
import signal

from .settings import *

class Pod(ABC): 
    def __init__(self, elmt) -> None:
        '''elmt is either an container, a process, ?'''
        self.last_readline_stdout = -1
        self.last_readline_stderr = -1

        self.elmt = elmt

    @abstractmethod
    async def _nextstdout(self):
        pass

    async def nextstdout(self):
        return {'stdout': await self._nextstdout()}

    @abstractmethod
    async def _nextstderr(self):
        pass

    async def nextstderr(self):
        return {'stderr': await self._nextstderr()}

    @abstractmethod
    async def stdout(self):
        pass

    @abstractmethod
    async def stderr(self):
        pass

    @abstractmethod
    async def terminate(self):
        pass

    @abstractmethod
    async def _is_terminated(self):
        pass

    async def is_terminated(self):
        return {'code': await self._is_terminated()}

def wrap2future(item):
    f = asyncio.Future()
    f.set_result(item)
    return f

class ProcessPod(Pod):
    def __init__(self, elmt) -> None:
        super().__init__(elmt)

    async def _nextstdout(self):
        line = await self.elmt.stdout.readline()
        return [line.decode()]

    async def _nextstderr(self):
        line = await self.elmt.stderr.readline()
        return [line.decode()]

    async def stderr(self):
        return (await self.elmt.stderr.read()).decode()

    async def stdout(self):
        return (await self.elmt.stdout.read()).decode()

    async def _is_terminated(self):
        if self.elmt.returncode == None:
            await asyncio.sleep(1) 
            return await self._is_terminated()
        else:
            return self.elmt.returncode 

    async def terminate(self):
        try:
            self.elmt.terminate()
            os.killpg(os.getpgid(self.elmt.pid), signal.SIGTERM)
            await self.elmt.wait()
            await asyncio.sleep(1)  # Needed to be able to bind ports afterwards
        except ProcessLookupError:
            # process has already exit (or crash)
            pass

class DockerPod(Pod):
    def __init__(self, elmt) -> None:
        super().__init__(elmt)

    async def nextof(self, _next):
        """retrun lines 
        subsequent call will not output the previously display lines"""

        await asyncio.sleep(1) # for Docker
        lines = await _next()

        if lines:
            lines = lines[self.last_readline_stderr:]
        self.last_readline_stderr += len(lines)

        return lines
    
    def _nextstdout(self):
        return self.nextof(lambda: self.elmt.log(stdout=True))

    def _nextstderr(self):
        return self.nextof(lambda: self.elmt.log(stderr=True))

    async def stdout(self):
        return ''.join(await self.elmt.log(stdout=True))

    async def stderr(self):
        return ''.join(await self.elmt.log(stderr=True))

    async def _is_terminated(self):
        return (await self.elmt.wait())["StatusCode"]

    async def terminate(self):
        return await self.elmt.delete(force=True)